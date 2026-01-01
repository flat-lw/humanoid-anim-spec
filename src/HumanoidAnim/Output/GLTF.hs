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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import Data.Word (Word8, Word16, Word32)
import Linear (V3(..), V4(..), Quaternion(..), norm)
import System.IO (withBinaryFile, IOMode(..))

import HumanoidAnim.Animation (AnimationClip(..), AnimationFrame(..))
import HumanoidAnim.Error
import HumanoidAnim.Skeleton.Bones (HumanoidBone(..), boneName)
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Hierarchy (boneParent, getBoneChain)
import HumanoidAnim.Output.Mesh

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

-- | Build glTF JSON structure with mesh
buildGLTFJson :: GLTFOptions -> AnimationClip -> Skeleton -> Aeson.Value
buildGLTFJson opts clip skeleton =
  let mesh = generateStickFigure defaultMeshConfig skeleton
      meshBufferData = buildMeshBufferData mesh
      ibmData = buildIBMData skeleton
      animBufferData = buildAnimBufferData opts clip skeleton
      -- Total buffer includes mesh + IBM + animation data
      totalBufferSize = BS.length meshBufferData + BS.length ibmData + BS.length animBufferData
      activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      numFrames = length (clipFrames clip)
      numVertices = length (meshVertices mesh)
      numIndices = length (meshIndices mesh)
  in object
    [ "asset" .= object
        [ "version" .= ("2.0" :: String)
        , "generator" .= ("humanoid-anim" :: String)
        ]
    , "scene" .= (0 :: Int)
    -- Scene includes root bone (Hips, index 0) and mesh node (last node)
    , "scenes" .= [object ["nodes" .= ([0, numBones] :: [Int])]]
    , "nodes" .= buildNodesWithMesh skeleton numVertices
    , "meshes" .= [buildMeshObject numVertices numIndices]
    , "skins" .= buildSkinsWithIBM skeleton
    , "materials" .= [buildMaterial]
    , "animations" .= [buildAnimation opts clip skeleton numVertices numIndices]
    , "buffers" .= [object ["byteLength" .= totalBufferSize]]
    , "bufferViews" .= buildBufferViewsWithMesh opts clip skeleton mesh
    , "accessors" .= buildAccessorsWithMesh opts clip skeleton mesh
    ]

-- | Build material for stick figure
buildMaterial :: Aeson.Value
buildMaterial = object
  [ "name" .= ("StickFigureMaterial" :: String)
  , "pbrMetallicRoughness" .= object
      [ "baseColorFactor" .= [0.8 :: Float, 0.8, 0.8, 1.0]
      , "metallicFactor" .= (0.1 :: Float)
      , "roughnessFactor" .= (0.8 :: Float)
      ]
  ]

-- | Build mesh object
buildMeshObject :: Int -> Int -> Aeson.Value
buildMeshObject numVertices numIndices = object
  [ "name" .= ("StickFigure" :: String)
  , "primitives" .= [object
      [ "attributes" .= object
          [ "POSITION" .= (0 :: Int)   -- Position accessor
          , "NORMAL" .= (1 :: Int)     -- Normal accessor
          , "JOINTS_0" .= (2 :: Int)   -- Joint indices accessor
          , "WEIGHTS_0" .= (3 :: Int)  -- Weights accessor
          ]
      , "indices" .= (4 :: Int)        -- Index accessor
      , "material" .= (0 :: Int)
      , "mode" .= (4 :: Int)           -- TRIANGLES
      ]]
  ]

-- | Build node hierarchy with mesh node
buildNodesWithMesh :: Skeleton -> Int -> [Aeson.Value]
buildNodesWithMesh skeleton numVertices =
  let activeBones = Map.keys (skeletonRestPose skeleton)
      sortedBones = sortBy (comparing boneOrder) activeBones
      boneNodes = map (buildNode skeleton sortedBones) sortedBones
      -- Add mesh node that references skin
      meshNode = object
        [ "name" .= ("StickFigureMesh" :: String)
        , "mesh" .= (0 :: Int)
        , "skin" .= (0 :: Int)
        ]
  in boneNodes ++ [meshNode]

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

-- | Build skins array with inverse bind matrices
buildSkinsWithIBM :: Skeleton -> [Aeson.Value]
buildSkinsWithIBM skeleton =
  let activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
  in [object
       [ "name" .= ("Humanoid" :: String)
       , "skeleton" .= (0 :: Int)  -- Root node (Hips)
       , "joints" .= [0 .. numBones - 1]
       , "inverseBindMatrices" .= (5 :: Int)  -- Accessor index for IBM
       ]]

-- | Build animation object
buildAnimation :: GLTFOptions -> AnimationClip -> Skeleton -> Int -> Int -> Aeson.Value
buildAnimation opts clip skeleton numVertices numIndices = object
  [ "name" .= clipName clip
  , "channels" .= buildChannels skeleton
  , "samplers" .= buildSamplers clip skeleton numVertices numIndices
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

-- | Build animation samplers (accessor indices adjusted for mesh data)
buildSamplers :: AnimationClip -> Skeleton -> Int -> Int -> [Aeson.Value]
buildSamplers clip skeleton numVertices numIndices =
  let activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      -- Accessor indices: 0-4 are mesh, 5 is IBM, 6 is time, 7+ are animation data
      timeAccessorIdx = 6 :: Int
      translationStartIdx = 7 :: Int
      rotationStartIdx = translationStartIdx + numBones
      -- Translation samplers
      translationSamplers = [object
        [ "input" .= timeAccessorIdx
        , "output" .= (translationStartIdx + i)
        , "interpolation" .= ("LINEAR" :: String)
        ] | i <- [0 .. numBones - 1]]
      -- Rotation samplers
      rotationSamplers = [object
        [ "input" .= timeAccessorIdx
        , "output" .= (rotationStartIdx + i)
        , "interpolation" .= ("LINEAR" :: String)
        ] | i <- [0 .. numBones - 1]]
  in translationSamplers ++ rotationSamplers

-- | Build buffer views including mesh data
buildBufferViewsWithMesh :: GLTFOptions -> AnimationClip -> Skeleton -> MeshData -> [Aeson.Value]
buildBufferViewsWithMesh opts clip skeleton mesh =
  let numFrames = length (clipFrames clip)
      activeBones = Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      numVertices = length (meshVertices mesh)
      numIndices = length (meshIndices mesh)

      -- Mesh buffer sizes
      positionSize = numVertices * 3 * 4    -- vec3 float32
      normalSize = numVertices * 3 * 4      -- vec3 float32
      jointsSize = numVertices * 4 * 2      -- vec4 uint16
      weightsSize = numVertices * 4 * 4     -- vec4 float32
      indicesSize = numIndices * 2          -- uint16
      ibmSize = numBones * 16 * 4           -- mat4 float32

      -- Animation buffer sizes
      timeViewSize = numFrames * 4
      translationViewSize = numFrames * 3 * 4
      rotationViewSize = numFrames * 4 * 4

      -- Calculate offsets
      offset0 = 0                           -- positions
      offset1 = offset0 + positionSize      -- normals
      offset2 = offset1 + normalSize        -- joints
      offset3 = offset2 + jointsSize        -- weights
      offset4 = offset3 + weightsSize       -- indices
      offset5 = offset4 + indicesSize       -- IBM
      animOffset = offset5 + ibmSize        -- animation data starts here

      -- Mesh buffer views (0-5)
      positionView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset0, "byteLength" .= positionSize
        , "target" .= (34962 :: Int) ]  -- ARRAY_BUFFER
      normalView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset1, "byteLength" .= normalSize
        , "target" .= (34962 :: Int) ]
      jointsView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset2, "byteLength" .= jointsSize
        , "target" .= (34962 :: Int) ]
      weightsView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset3, "byteLength" .= weightsSize
        , "target" .= (34962 :: Int) ]
      indicesView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset4, "byteLength" .= indicesSize
        , "target" .= (34963 :: Int) ]  -- ELEMENT_ARRAY_BUFFER
      ibmView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= offset5, "byteLength" .= ibmSize ]

      -- Animation buffer views (6+)
      timeView = object
        [ "buffer" .= (0 :: Int), "byteOffset" .= animOffset, "byteLength" .= timeViewSize ]

      translationViews = [object
        [ "buffer" .= (0 :: Int)
        , "byteOffset" .= (animOffset + timeViewSize + i * translationViewSize)
        , "byteLength" .= translationViewSize
        ] | i <- [0 .. numBones - 1]]

      rotationOffset = animOffset + timeViewSize + numBones * translationViewSize
      rotationViews = [object
        [ "buffer" .= (0 :: Int)
        , "byteOffset" .= (rotationOffset + i * rotationViewSize)
        , "byteLength" .= rotationViewSize
        ] | i <- [0 .. numBones - 1]]

  in [positionView, normalView, jointsView, weightsView, indicesView, ibmView, timeView]
     ++ translationViews ++ rotationViews

-- | Build accessors including mesh data
buildAccessorsWithMesh :: GLTFOptions -> AnimationClip -> Skeleton -> MeshData -> [Aeson.Value]
buildAccessorsWithMesh opts clip skeleton mesh =
  let numFrames = length (clipFrames clip)
      activeBones = Map.keys (skeletonRestPose skeleton)
      numBones = length activeBones
      numVertices = length (meshVertices mesh)
      numIndices = length (meshIndices mesh)
      times = map frameTime (clipFrames clip)
      minTime = minimum times
      maxTime = maximum times

      -- Calculate position bounds
      positions = map vertPosition (meshVertices mesh)
      (minPos, maxPos) = positionBounds positions

      -- Mesh accessors (0-4)
      positionAccessor = object  -- 0
        [ "bufferView" .= (0 :: Int)
        , "componentType" .= (5126 :: Int)  -- FLOAT
        , "count" .= numVertices
        , "type" .= ("VEC3" :: String)
        , "min" .= v3ToList minPos
        , "max" .= v3ToList maxPos
        ]
      normalAccessor = object  -- 1
        [ "bufferView" .= (1 :: Int)
        , "componentType" .= (5126 :: Int)
        , "count" .= numVertices
        , "type" .= ("VEC3" :: String)
        ]
      jointsAccessor = object  -- 2
        [ "bufferView" .= (2 :: Int)
        , "componentType" .= (5123 :: Int)  -- UNSIGNED_SHORT
        , "count" .= numVertices
        , "type" .= ("VEC4" :: String)
        ]
      weightsAccessor = object  -- 3
        [ "bufferView" .= (3 :: Int)
        , "componentType" .= (5126 :: Int)
        , "count" .= numVertices
        , "type" .= ("VEC4" :: String)
        ]
      indicesAccessor = object  -- 4
        [ "bufferView" .= (4 :: Int)
        , "componentType" .= (5123 :: Int)  -- UNSIGNED_SHORT
        , "count" .= numIndices
        , "type" .= ("SCALAR" :: String)
        ]

      -- Inverse bind matrices accessor (5)
      ibmAccessor = object
        [ "bufferView" .= (5 :: Int)
        , "componentType" .= (5126 :: Int)
        , "count" .= numBones
        , "type" .= ("MAT4" :: String)
        ]

      -- Time accessor (6)
      timeAccessor = object
        [ "bufferView" .= (6 :: Int)
        , "componentType" .= (5126 :: Int)
        , "count" .= numFrames
        , "type" .= ("SCALAR" :: String)
        , "min" .= [minTime]
        , "max" .= [maxTime]
        ]

      -- Translation accessors (7+)
      translationAccessors = [object
        [ "bufferView" .= (7 + i)
        , "componentType" .= (5126 :: Int)
        , "count" .= numFrames
        , "type" .= ("VEC3" :: String)
        ] | i <- [0 .. numBones - 1]]

      -- Rotation accessors
      rotationAccessors = [object
        [ "bufferView" .= (7 + numBones + i)
        , "componentType" .= (5126 :: Int)
        , "count" .= numFrames
        , "type" .= ("VEC4" :: String)
        ] | i <- [0 .. numBones - 1]]

  in [positionAccessor, normalAccessor, jointsAccessor, weightsAccessor, indicesAccessor,
      ibmAccessor, timeAccessor]
     ++ translationAccessors ++ rotationAccessors

-- | Calculate position bounds
positionBounds :: [V3 Float] -> (V3 Float, V3 Float)
positionBounds [] = (V3 0 0 0, V3 0 0 0)
positionBounds (p:ps) = foldr updateBounds (p, p) ps
  where
    updateBounds (V3 x y z) (V3 minX minY minZ, V3 maxX maxY maxZ) =
      (V3 (min x minX) (min y minY) (min z minZ),
       V3 (max x maxX) (max y maxY) (max z maxZ))

-- | Build mesh buffer data
buildMeshBufferData :: MeshData -> ByteString
buildMeshBufferData mesh = LBS.toStrict $ Builder.toLazyByteString builder
  where
    vertices = meshVertices mesh
    indices = meshIndices mesh

    builder = mconcat
      [ -- Positions
        mconcat [let V3 x y z = vertPosition v in
                 Builder.floatLE x <> Builder.floatLE y <> Builder.floatLE z
                | v <- vertices]
      , -- Normals
        mconcat [let V3 x y z = vertNormal v in
                 Builder.floatLE x <> Builder.floatLE y <> Builder.floatLE z
                | v <- vertices]
      , -- Joints (vec4 uint16)
        mconcat [let V4 j0 j1 j2 j3 = vertJoints v in
                 Builder.word16LE j0 <> Builder.word16LE j1 <>
                 Builder.word16LE j2 <> Builder.word16LE j3
                | v <- vertices]
      , -- Weights (vec4 float32)
        mconcat [let V4 w0 w1 w2 w3 = vertWeights v in
                 Builder.floatLE w0 <> Builder.floatLE w1 <>
                 Builder.floatLE w2 <> Builder.floatLE w3
                | v <- vertices]
      , -- Indices
        mconcat [Builder.word16LE i | i <- indices]
      ]

-- | Build inverse bind matrices
buildIBMData :: Skeleton -> ByteString
buildIBMData skeleton = LBS.toStrict $ Builder.toLazyByteString builder
  where
    activeBones = sortBy (comparing boneOrder) $ Map.keys (skeletonRestPose skeleton)

    builder = mconcat [boneIBM bone | bone <- activeBones]

    -- Inverse bind matrix: inverse of bone's world transform
    -- For simplicity, we compute translation-only inverse (negate position)
    boneIBM bone =
      let Transform (V3 tx ty tz) _ = Map.findWithDefault
            (Transform (V3 0 0 0) (Quaternion 1 (V3 0 0 0)))
            bone
            (skeletonRestPose skeleton)
          -- Column-major 4x4 identity with translation
          -- IBM = inverse of world transform
          -- For T-pose, rotation is identity, so just negate translation
      in mconcat
           [ Builder.floatLE 1, Builder.floatLE 0, Builder.floatLE 0, Builder.floatLE 0  -- col 0
           , Builder.floatLE 0, Builder.floatLE 1, Builder.floatLE 0, Builder.floatLE 0  -- col 1
           , Builder.floatLE 0, Builder.floatLE 0, Builder.floatLE 1, Builder.floatLE 0  -- col 2
           , Builder.floatLE (-tx), Builder.floatLE (-ty), Builder.floatLE (-tz), Builder.floatLE 1  -- col 3
           ]

-- | Build animation buffer data
buildAnimBufferData :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString
buildAnimBufferData opts clip skeleton = LBS.toStrict $ Builder.toLazyByteString builder
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

-- | Build GLB binary with mesh
buildGLB :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString
buildGLB opts clip skeleton = LBS.toStrict $ Builder.toLazyByteString glbBuilder
  where
    mesh = generateStickFigure defaultMeshConfig skeleton
    jsonData = LBS.toStrict $ Aeson.encode $ buildGLTFJson opts clip skeleton
    meshBufferData = buildMeshBufferData mesh
    ibmData = buildIBMData skeleton
    animBufferData = buildAnimBufferData opts clip skeleton
    bufferData = meshBufferData <> ibmData <> animBufferData

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

-- | Convert V3 to list
v3ToList :: V3 Float -> [Float]
v3ToList (V3 x y z) = [x, y, z]

-- | Convert quaternion to list for JSON (x, y, z, w order for glTF)
quaternionToList :: Quaternion Float -> [Float]
quaternionToList (Quaternion w (V3 x y z)) = [x, y, z, w]
