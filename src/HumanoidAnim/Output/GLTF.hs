{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : HumanoidAnim.Output.GLTF
-- Description : glTF/GLB output
--
-- This module handles exporting animation clips to glTF and GLB formats
-- for use in Unity and other 3D applications.
module HumanoidAnim.Output.GLTF
  ( -- * Output Options
    GLTFOptions(..)
  , GLTFFormat(..)
  , defaultGLTFOptions

    -- * Export Functions
  , exportGLTF
  , writeGLTF

    -- * Low-level
  , buildGLTFJson
  , buildGLB
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import Data.Word (Word8, Word32)
import Linear (V3(..), Quaternion(..))
import System.IO (withBinaryFile, IOMode(..))

import HumanoidAnim.Animation (AnimationClip(..), AnimationFrame(..))
import HumanoidAnim.Error
import HumanoidAnim.Skeleton.Bones (HumanoidBone(..), boneName)
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Hierarchy (boneParent, getBoneChain)

-- | glTF output format
data GLTFFormat = GLB | GLTF
  deriving stock (Show, Eq, Read, Enum, Bounded)

-- | glTF export options
data GLTFOptions = GLTFOptions
  { gltfFormat :: GLTFFormat
    -- ^ Output format (GLB or GLTF)
  , gltfPrecision :: Int
    -- ^ Decimal precision for values
  , gltfOptimize :: Bool
    -- ^ Optimize keyframes (remove redundant)
  } deriving stock (Show, Eq)

-- | Default glTF options
defaultGLTFOptions :: GLTFOptions
defaultGLTFOptions = GLTFOptions
  { gltfFormat = GLB
  , gltfPrecision = 6
  , gltfOptimize = True
  }

-- | Export animation clip to ByteString
exportGLTF :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString
exportGLTF opts clip skeleton =
  case gltfFormat opts of
    GLTF -> LBS.toStrict $ Aeson.encodePretty $ buildGLTFJson opts clip skeleton
    GLB -> buildGLB opts clip skeleton

-- | Write animation to file
writeGLTF :: FilePath -> GLTFOptions -> AnimationClip -> Skeleton -> IO (Result ())
writeGLTF path opts clip skeleton = do
  let content = exportGLTF opts clip skeleton
  result <- tryWriteFile path content
  pure $ case result of
    Left err -> failure $ OutputWriteError path err
    Right () -> success ()

-- | Try to write file, catching exceptions
tryWriteFile :: FilePath -> ByteString -> IO (Either String ())
tryWriteFile path content = do
  BS.writeFile path content
  pure (Right ())

-- | Build glTF JSON structure
buildGLTFJson :: GLTFOptions -> AnimationClip -> Skeleton -> Aeson.Value
buildGLTFJson opts clip skeleton = object
  [ "asset" .= object
      [ "version" .= ("2.0" :: String)
      , "generator" .= ("humanoid-anim" :: String)
      ]
  , "scene" .= (0 :: Int)
  , "scenes" .= [object ["nodes" .= [0 :: Int]]]
  , "nodes" .= buildNodes skeleton
  , "skins" .= buildSkins skeleton
  , "animations" .= [buildAnimation opts clip skeleton]
  , "buffers" .= buildBuffers opts clip skeleton
  , "bufferViews" .= buildBufferViews opts clip skeleton
  , "accessors" .= buildAccessors opts clip skeleton
  ]

-- | Build node hierarchy for skeleton
buildNodes :: Skeleton -> [Aeson.Value]
buildNodes skeleton =
  let activeBones = Map.keys (skeletonRestPose skeleton)
      sortedBones = sortBy (comparing boneOrder) activeBones
  in map (buildNode skeleton sortedBones) sortedBones

-- | Order bones for consistent output
boneOrder :: HumanoidBone -> Int
boneOrder = fromEnum

-- | Build a single node
buildNode :: Skeleton -> [HumanoidBone] -> HumanoidBone -> Aeson.Value
buildNode skeleton allBones bone = object $
  [ "name" .= boneName bone
  , "translation" .= positionToList pos
  , "rotation" .= quaternionToList rot
  ] ++ childrenField
  where
    Transform pos rot = Map.findWithDefault
      (Transform (V3 0 0 0) (Quaternion 1 (V3 0 0 0)))
      bone
      (skeletonRestPose skeleton)

    children = [b | b <- allBones, boneParent b == Just bone]
    childrenField = if null children
      then []
      else ["children" .= map (boneIndex allBones) children]

-- | Get bone index in list
boneIndex :: [HumanoidBone] -> HumanoidBone -> Int
boneIndex bones bone = case lookup bone (zip bones [0..]) of
  Just i -> i
  Nothing -> 0

-- | Build skins array
buildSkins :: Skeleton -> [Aeson.Value]
buildSkins skeleton =
  let activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
  in [object
       [ "name" .= ("Humanoid" :: String)
       , "skeleton" .= (0 :: Int)  -- Root node
       , "joints" .= [0 .. length activeBones - 1]
       ]]

-- | Build animation object
buildAnimation :: GLTFOptions -> AnimationClip -> Skeleton -> Aeson.Value
buildAnimation opts clip skeleton = object
  [ "name" .= clipName clip
  , "channels" .= buildChannels skeleton
  , "samplers" .= buildSamplers clip skeleton
  ]

-- | Build animation channels
buildChannels :: Skeleton -> [Aeson.Value]
buildChannels skeleton =
  let activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
      translationChannels = zipWith (buildChannel "translation") [0..] activeBones
      rotationChannels = zipWith (buildChannel "rotation") [length activeBones..] activeBones
  in translationChannels ++ rotationChannels

-- | Build a single channel
buildChannel :: String -> Int -> HumanoidBone -> Aeson.Value
buildChannel path samplerIdx bone = object
  [ "sampler" .= samplerIdx
  , "target" .= object
      [ "node" .= fromEnum bone
      , "path" .= path
      ]
  ]

-- | Build animation samplers
buildSamplers :: AnimationClip -> Skeleton -> [Aeson.Value]
buildSamplers clip skeleton =
  let activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      -- Translation samplers
      translationSamplers = [object
        [ "input" .= (0 :: Int)  -- Time accessor
        , "output" .= (i + 1)    -- Translation accessor
        , "interpolation" .= ("LINEAR" :: String)
        ] | i <- [0 .. numBones - 1]]
      -- Rotation samplers
      rotationSamplers = [object
        [ "input" .= (0 :: Int)
        , "output" .= (numBones + 1 + i)  -- Rotation accessor
        , "interpolation" .= ("LINEAR" :: String)
        ] | i <- [0 .. numBones - 1]]
  in translationSamplers ++ rotationSamplers

-- | Build buffers array
buildBuffers :: GLTFOptions -> AnimationClip -> Skeleton -> [Aeson.Value]
buildBuffers opts clip skeleton =
  let bufferData = buildBufferData opts clip skeleton
  in [object
       [ "byteLength" .= BS.length bufferData
       ]]

-- | Build buffer views
buildBufferViews :: GLTFOptions -> AnimationClip -> Skeleton -> [Aeson.Value]
buildBufferViews opts clip skeleton =
  let numFrames = length (clipFrames clip)
      activeBones = Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones

      timeViewSize = numFrames * 4  -- float32
      translationViewSize = numFrames * 3 * 4  -- vec3 * float32
      rotationViewSize = numFrames * 4 * 4  -- vec4 * float32

      -- Time view
      timeView = object
        [ "buffer" .= (0 :: Int)
        , "byteOffset" .= (0 :: Int)
        , "byteLength" .= timeViewSize
        ]

      -- Translation views
      translationViews = [object
        [ "buffer" .= (0 :: Int)
        , "byteOffset" .= (timeViewSize + i * translationViewSize)
        , "byteLength" .= translationViewSize
        ] | i <- [0 .. numBones - 1]]

      -- Rotation views
      rotationOffset = timeViewSize + numBones * translationViewSize
      rotationViews = [object
        [ "buffer" .= (0 :: Int)
        , "byteOffset" .= (rotationOffset + i * rotationViewSize)
        , "byteLength" .= rotationViewSize
        ] | i <- [0 .. numBones - 1]]

  in [timeView] ++ translationViews ++ rotationViews

-- | Build accessors
buildAccessors :: GLTFOptions -> AnimationClip -> Skeleton -> [Aeson.Value]
buildAccessors opts clip skeleton =
  let numFrames = length (clipFrames clip)
      activeBones = Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      times = map frameTime (clipFrames clip)
      minTime = minimum times
      maxTime = maximum times

      -- Time accessor
      timeAccessor = object
        [ "bufferView" .= (0 :: Int)
        , "componentType" .= (5126 :: Int)  -- FLOAT
        , "count" .= numFrames
        , "type" .= ("SCALAR" :: String)
        , "min" .= [minTime]
        , "max" .= [maxTime]
        ]

      -- Translation accessors
      translationAccessors = [object
        [ "bufferView" .= (i + 1)
        , "componentType" .= (5126 :: Int)
        , "count" .= numFrames
        , "type" .= ("VEC3" :: String)
        ] | i <- [0 .. numBones - 1]]

      -- Rotation accessors
      rotationAccessors = [object
        [ "bufferView" .= (numBones + 1 + i)
        , "componentType" .= (5126 :: Int)
        , "count" .= numFrames
        , "type" .= ("VEC4" :: String)
        ] | i <- [0 .. numBones - 1]]

  in [timeAccessor] ++ translationAccessors ++ rotationAccessors

-- | Build binary buffer data
buildBufferData :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString
buildBufferData opts clip skeleton = LBS.toStrict $ Builder.toLazyByteString builder
  where
    frames = clipFrames clip
    activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)

    builder = mconcat
      [ -- Times
        mconcat [Builder.floatLE (frameTime f) | f <- frames]
        -- Translations for each bone
      , mconcat [boneTranslations bone | bone <- activeBones]
        -- Rotations for each bone
      , mconcat [boneRotations bone | bone <- activeBones]
      ]

    boneTranslations bone = mconcat
      [ case Map.lookup bone (framePose f) of
          Just (Transform (V3 x y z) _) ->
            Builder.floatLE x <> Builder.floatLE y <> Builder.floatLE z
          Nothing ->
            Builder.floatLE 0 <> Builder.floatLE 0 <> Builder.floatLE 0
      | f <- frames
      ]

    boneRotations bone = mconcat
      [ case Map.lookup bone (framePose f) of
          Just (Transform _ (Quaternion w (V3 x y z))) ->
            Builder.floatLE x <> Builder.floatLE y <> Builder.floatLE z <> Builder.floatLE w
          Nothing ->
            Builder.floatLE 0 <> Builder.floatLE 0 <> Builder.floatLE 0 <> Builder.floatLE 1
      | f <- frames
      ]

-- | Build GLB binary
buildGLB :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString
buildGLB opts clip skeleton = LBS.toStrict $ Builder.toLazyByteString glbBuilder
  where
    jsonData = LBS.toStrict $ Aeson.encode $ buildGLTFJson opts clip skeleton
    bufferData = buildBufferData opts clip skeleton

    -- Pad JSON to 4-byte boundary
    jsonPadding = (4 - BS.length jsonData `mod` 4) `mod` 4
    paddedJsonLength = BS.length jsonData + jsonPadding

    -- Pad buffer to 4-byte boundary
    bufferPadding = (4 - BS.length bufferData `mod` 4) `mod` 4
    paddedBufferLength = BS.length bufferData + bufferPadding

    -- Total file size
    totalLength = 12 + 8 + paddedJsonLength + 8 + paddedBufferLength

    glbBuilder = mconcat
      [ -- GLB header
        Builder.word32LE 0x46546C67  -- "glTF" magic
      , Builder.word32LE 2           -- Version
      , Builder.word32LE (fromIntegral totalLength)
        -- JSON chunk
      , Builder.word32LE (fromIntegral paddedJsonLength)
      , Builder.word32LE 0x4E4F534A  -- "JSON"
      , Builder.byteString jsonData
      , Builder.byteString (BS.replicate jsonPadding 0x20)  -- Space padding
        -- BIN chunk
      , Builder.word32LE (fromIntegral paddedBufferLength)
      , Builder.word32LE 0x004E4942  -- "BIN\0"
      , Builder.byteString bufferData
      , Builder.byteString (BS.replicate bufferPadding 0x00)
      ]

-- | Convert position to list for JSON
positionToList :: V3 Float -> [Float]
positionToList (V3 x y z) = [x, y, z]

-- | Convert quaternion to list for JSON (x, y, z, w order for glTF)
quaternionToList :: Quaternion Float -> [Float]
quaternionToList (Quaternion w (V3 x y z)) = [x, y, z, w]
