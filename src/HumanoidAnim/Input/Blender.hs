{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : HumanoidAnim.Input.Blender
-- Description : Blender JSON format importer
--
-- This module handles importing animation data from Blender's JSON export format
-- and converting it to the internal configuration format.
module HumanoidAnim.Input.Blender
  ( -- * Blender Types
    BlenderAnimation(..)
  , BlenderKeyframe(..)
  , BlenderBone(..)

    -- * Import Functions
  , parseBlenderJson
  , loadBlenderJson
  , blenderToConfig

    -- * Accessors
  , blenderAnimName
  , blenderAnimFrameStart
  , blenderAnimFrameEnd
  , blenderAnimFps

    -- * Coordinate Conversion
  , convertZUpToYUp
  , convertYUpToZUp
  ) where

import Control.Monad (forM)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Linear (V3(..), Quaternion(..), V4(..))

import HumanoidAnim.Error
import HumanoidAnim.Input.Config hiding (keyframeTime, keyframePosition, keyframeRotation, keyframeEasing)
import qualified HumanoidAnim.Input.Config as C
import HumanoidAnim.Motion.Easing (Easing(..))
import HumanoidAnim.Skeleton.Bones hiding (boneName)

-- | Blender animation data
data BlenderAnimation = BlenderAnimation
  { blenderName :: Text
  , blenderFrameStart :: Int
  , blenderFrameEnd :: Int
  , blenderFps :: Float
  , blenderBones :: [BlenderBone]
  } deriving stock (Show, Eq, Generic)

instance FromJSON BlenderAnimation where
  parseJSON = withObject "BlenderAnimation" $ \o -> do
    name <- o .: "name"
    frameStart <- o .:? "frame_start" .!= 0
    frameEnd <- o .: "frame_end"
    fps <- o .:? "fps" .!= 24.0
    bones <- o .: "bones"
    return BlenderAnimation
      { blenderName = name
      , blenderFrameStart = frameStart
      , blenderFrameEnd = frameEnd
      , blenderFps = fps
      , blenderBones = bones
      }

-- | Blender bone animation data
data BlenderBone = BlenderBone
  { boneName :: Text
  , boneKeyframes :: [BlenderKeyframe]
  } deriving stock (Show, Eq, Generic)

instance FromJSON BlenderBone where
  parseJSON = withObject "BlenderBone" $ \o -> do
    name <- o .: "name"
    keyframes <- o .:? "keyframes" .!= []
    return BlenderBone
      { boneName = name
      , boneKeyframes = keyframes
      }

-- | Blender keyframe data
data BlenderKeyframe = BlenderKeyframe
  { keyframeFrame :: Int
  , keyframeLocation :: Maybe (V3 Float)
  , keyframeRotation :: Maybe (V4 Float)  -- Quaternion as [x, y, z, w] or [w, x, y, z]
  , keyframeScale :: Maybe (V3 Float)
  } deriving stock (Show, Eq, Generic)

instance FromJSON BlenderKeyframe where
  parseJSON = withObject "BlenderKeyframe" $ \o -> do
    frame <- o .: "frame"
    loc <- o .:? "location"
    rot <- o .:? "rotation"  -- Could be quaternion or euler
    scale <- o .:? "scale"
    return BlenderKeyframe
      { keyframeFrame = frame
      , keyframeLocation = loc
      , keyframeRotation = rot
      , keyframeScale = scale
      }

instance FromJSON (V3 Float) where
  parseJSON = withArray "V3" $ \arr ->
    if length arr == 3
    then do
      x <- parseJSON (arr `indexV` 0)
      y <- parseJSON (arr `indexV` 1)
      z <- parseJSON (arr `indexV` 2)
      return (V3 x y z)
    else fail "V3 requires exactly 3 elements"
    where
      indexV arr i = arr `seq` (toList arr !! i)
      toList = foldr (:) []

instance FromJSON (V4 Float) where
  parseJSON = withArray "V4" $ \arr ->
    if length arr == 4
    then do
      a <- parseJSON (arr `indexV` 0)
      b <- parseJSON (arr `indexV` 1)
      c <- parseJSON (arr `indexV` 2)
      d <- parseJSON (arr `indexV` 3)
      return (V4 a b c d)
    else fail "V4 requires exactly 4 elements"
    where
      indexV arr i = arr `seq` (toList arr !! i)
      toList = foldr (:) []

-- | Parse Blender JSON from ByteString
parseBlenderJson :: BS.ByteString -> Either String BlenderAnimation
parseBlenderJson bs = eitherDecodeStrict bs

-- | Load Blender JSON from file
loadBlenderJson :: FilePath -> IO (Result BlenderAnimation)
loadBlenderJson path = do
  content <- BS.readFile path
  case parseBlenderJson content of
    Left err -> pure $ failure $ ConfigReadError path err
    Right anim -> pure $ success anim

-- | Convert Blender animation to internal config format
blenderToConfig :: BlenderAnimation -> Result AnimationConfig
blenderToConfig anim = do
  let duration = fromIntegral (blenderFrameEnd anim - blenderFrameStart anim) / blenderFps anim
      fps = blenderFps anim
      startFrame = blenderFrameStart anim

      -- Find effector bone (usually the last in the list or one with "hand" in name)
      effectorBoneData = findEffectorBone (blenderBones anim)

  case effectorBoneData of
    Nothing -> failure $ MissingField "No suitable effector bone found"
    Just (boneData, humanBone) ->
      let keyframes = convertKeyframes fps startFrame (boneKeyframes boneData)
      in if length keyframes < 2
         then failure $ InsufficientKeyframes (length keyframes)
         else success AnimationConfig
           { configName = blenderName anim
           , configDuration = duration
           , configFixed =
               [ FixedBoneConfig "Hips" (V3 0 1 0) Nothing  -- Default fixed bones
               ]
           , configEffector = EffectorConfig
               { effectorBone = T.pack (show humanBone)
               , effectorKeyframes = keyframes
               }
           , configSettings = defaultGenerationSettings
               { settingsFrameRate = fps
               , settingsLoop = "once"
               }
           , configSkeleton = defaultSkeletonSettings
           , configOutput = defaultOutputSettings
           , configInput = defaultInputSettings
           }

-- | Find a suitable effector bone from Blender bones
findEffectorBone :: [BlenderBone] -> Maybe (BlenderBone, HumanoidBone)
findEffectorBone bones =
  let -- Priority order for effector bones
      priorities =
        [ ("hand.r", RightHand)
        , ("hand.l", LeftHand)
        , ("hand_r", RightHand)
        , ("hand_l", LeftHand)
        , ("RightHand", RightHand)
        , ("LeftHand", LeftHand)
        , ("foot.r", RightFoot)
        , ("foot.l", LeftFoot)
        , ("foot_r", RightFoot)
        , ("foot_l", LeftFoot)
        , ("RightFoot", RightFoot)
        , ("LeftFoot", LeftFoot)
        ]

      findMatch [] = Nothing
      findMatch ((pattern, hBone):rest) =
        case filter (matchBoneName pattern . boneName) bones of
          (b:_) -> Just (b, hBone)
          [] -> findMatch rest

      matchBoneName pattern name =
        T.toLower pattern `T.isInfixOf` T.toLower name

  in findMatch priorities

-- | Convert Blender keyframes to internal format
convertKeyframes :: Float -> Int -> [BlenderKeyframe] -> [C.KeyframeConfig]
convertKeyframes fps startFrame = mapMaybe convert
  where
    convert kf =
      case keyframeLocation kf of
        Nothing -> Nothing
        Just loc ->
          let time = fromIntegral (keyframeFrame kf - startFrame) / fps
              -- Convert Z-up to Y-up
              pos = convertZUpToYUp loc
              rot = fmap convertRotationZUpToYUp (keyframeRotation kf)
          in Just C.KeyframeConfig
               { C.kfTime = time
               , C.kfPosition = pos
               , C.kfRotation = fmap v4ToQuat rot
               , C.kfEasing = Linear
               }

-- | Convert V4 to Quaternion
v4ToQuat :: V4 Float -> Quaternion Float
v4ToQuat (V4 x y z w) = Quaternion w (V3 x y z)

-- | Convert position from Z-up (Blender) to Y-up (Unity/glTF)
convertZUpToYUp :: V3 Float -> V3 Float
convertZUpToYUp (V3 x y z) = V3 x z (-y)

-- | Convert position from Y-up to Z-up
convertYUpToZUp :: V3 Float -> V3 Float
convertYUpToZUp (V3 x y z) = V3 x (-z) y

-- | Convert rotation quaternion from Z-up to Y-up
convertRotationZUpToYUp :: V4 Float -> V4 Float
convertRotationZUpToYUp (V4 x y z w) =
  -- Rotate by 90 degrees around X axis
  let rotX = V4 (sqrt 0.5) 0 0 (sqrt 0.5)  -- 90 degree X rotation
  in multiplyQuatV4 rotX (V4 x z (-y) w)

-- | Multiply two quaternions (as V4)
multiplyQuatV4 :: V4 Float -> V4 Float -> V4 Float
multiplyQuatV4 (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 x y z w
  where
    w = w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2
    x = w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2
    y = w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2
    z = w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2

-- | Convert V3 to list
v3ToList :: V3 Float -> [Float]
v3ToList (V3 x y z) = [x, y, z]

-- | Convert V4 to list
v4ToList :: V4 Float -> [Float]
v4ToList (V4 x y z w) = [x, y, z, w]

-- | Get animation name
blenderAnimName :: BlenderAnimation -> Text
blenderAnimName = blenderName

-- | Get frame start
blenderAnimFrameStart :: BlenderAnimation -> Int
blenderAnimFrameStart = blenderFrameStart

-- | Get frame end
blenderAnimFrameEnd :: BlenderAnimation -> Int
blenderAnimFrameEnd = blenderFrameEnd

-- | Get FPS
blenderAnimFps :: BlenderAnimation -> Float
blenderAnimFps = blenderFps
