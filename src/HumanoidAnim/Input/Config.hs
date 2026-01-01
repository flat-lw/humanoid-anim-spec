{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : HumanoidAnim.Input.Config
-- Description : YAML/JSON configuration file parser
--
-- This module handles parsing of animation configuration files in YAML
-- and JSON formats, converting them to internal data structures.
module HumanoidAnim.Input.Config
  ( -- * Configuration Types
    AnimationConfig(..)
  , FixedBoneConfig(..)
  , EffectorConfig(..)
  , KeyframeConfig(..)
  , GenerationSettings(..)
  , SkeletonSettings(..)
  , OutputSettings(..)
  , InputSettings(..)

    -- * Parsing
  , loadConfig
  , loadConfigFromFile
  , parseConfig

    -- * Writing
  , writeConfigToFile

    -- * Simplified Config (for Blender import)
  , ConfigOptions(..)

    -- * Defaults
  , defaultGenerationSettings
  , defaultSkeletonSettings
  , defaultOutputSettings
  , defaultInputSettings
  ) where

import Control.Monad (when)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , (.:?)
  , (.!=)
  , withObject
  , withText
  , object
  , (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Linear (V3(..), Quaternion(..))
import System.FilePath (takeExtension)

import HumanoidAnim.Error
import HumanoidAnim.Motion.Easing (Easing(..), parseEasing)
import HumanoidAnim.Skeleton.Bones (SkeletonDetail(..), parseBoneName)

-- | Main animation configuration
data AnimationConfig = AnimationConfig
  { configName :: Text
    -- ^ Animation name
  , configDuration :: Float
    -- ^ Duration in seconds
  , configFixed :: [FixedBoneConfig]
    -- ^ Fixed bone constraints
  , configEffector :: EffectorConfig
    -- ^ Effector bone and keyframes
  , configSettings :: GenerationSettings
    -- ^ Generation settings
  , configSkeleton :: SkeletonSettings
    -- ^ Skeleton settings
  , configOutput :: OutputSettings
    -- ^ Output settings
  , configInput :: InputSettings
    -- ^ Input coordinate system settings
  } deriving stock (Show, Eq)

-- | Fixed bone configuration
data FixedBoneConfig = FixedBoneConfig
  { fixedBone :: Text
    -- ^ Bone name
  , fixedPosition :: V3 Float
    -- ^ Fixed position [x, y, z]
  , fixedRotation :: Maybe (Quaternion Float)
    -- ^ Optional fixed rotation [x, y, z, w]
  } deriving stock (Show, Eq)

-- | Effector configuration
data EffectorConfig = EffectorConfig
  { effectorBone :: Text
    -- ^ Effector bone name
  , effectorKeyframes :: [KeyframeConfig]
    -- ^ Keyframe list
  } deriving stock (Show, Eq)

-- | Keyframe configuration
data KeyframeConfig = KeyframeConfig
  { kfTime :: Float
    -- ^ Time in seconds
  , kfPosition :: V3 Float
    -- ^ Position [x, y, z]
  , kfRotation :: Maybe (Quaternion Float)
    -- ^ Optional rotation [x, y, z, w]
  , kfEasing :: Easing
    -- ^ Easing function
  } deriving stock (Show)

-- Manual Eq instance since Easing doesn't always have Eq for Custom
instance Eq KeyframeConfig where
  k1 == k2 = kfTime k1 == kfTime k2
          && kfPosition k1 == kfPosition k2
          && kfRotation k1 == kfRotation k2

-- | Generation settings
data GenerationSettings = GenerationSettings
  { settingsFrameRate :: Float
    -- ^ Frame rate (fps)
  , settingsFrameCount :: Maybe Int
    -- ^ Optional fixed frame count
  , settingsSolver :: Text
    -- ^ Solver name: "fabrik" | "ccd" | "hybrid"
  , settingsLoop :: Text
    -- ^ Loop mode: "once" | "cycle" | "pingpong"
  , settingsRootMotion :: Text
    -- ^ Root motion: "fixed" | "free" | "y-only"
  } deriving stock (Show, Eq)

-- | Skeleton settings
data SkeletonSettings = SkeletonSettings
  { skelDetail :: SkeletonDetail
    -- ^ Skeleton detail level
  , skelBoneLengths :: [(Text, Float)]
    -- ^ Custom bone lengths
  } deriving stock (Show, Eq)

-- | Output settings
data OutputSettings = OutputSettings
  { outFormat :: Text
    -- ^ Output format: "glb" | "gltf"
  , outOptimize :: Bool
    -- ^ Keyframe optimization
  , outPrecision :: Int
    -- ^ Decimal precision
  } deriving stock (Show, Eq)

-- | Input settings
data InputSettings = InputSettings
  { inCoordinateSystem :: Text
    -- ^ Coordinate system: "y-up" | "z-up"
  } deriving stock (Show, Eq)

-- Default values

defaultGenerationSettings :: GenerationSettings
defaultGenerationSettings = GenerationSettings
  { settingsFrameRate = 30
  , settingsFrameCount = Nothing
  , settingsSolver = "fabrik"
  , settingsLoop = "once"
  , settingsRootMotion = "fixed"
  }

defaultSkeletonSettings :: SkeletonSettings
defaultSkeletonSettings = SkeletonSettings
  { skelDetail = StandardSkeleton
  , skelBoneLengths = []
  }

defaultOutputSettings :: OutputSettings
defaultOutputSettings = OutputSettings
  { outFormat = "glb"
  , outOptimize = True
  , outPrecision = 6
  }

defaultInputSettings :: InputSettings
defaultInputSettings = InputSettings
  { inCoordinateSystem = "y-up"
  }

-- JSON/YAML instances

instance FromJSON AnimationConfig where
  parseJSON = withObject "AnimationConfig" $ \o -> do
    name <- o .: "name"
    duration <- o .: "duration"
    fixed <- o .: "fixed"
    effector <- o .: "effector"
    config <- o .:? "config" .!= Aeson.Object mempty
    skeleton <- o .:? "skeleton" .!= Aeson.Object mempty
    output <- o .:? "output" .!= Aeson.Object mempty
    input <- o .:? "input" .!= Aeson.Object mempty

    settings <- parseJSON config
    skelSettings <- parseJSON skeleton
    outSettings <- parseJSON output
    inSettings <- parseJSON input

    pure AnimationConfig
      { configName = name
      , configDuration = duration
      , configFixed = fixed
      , configEffector = effector
      , configSettings = settings
      , configSkeleton = skelSettings
      , configOutput = outSettings
      , configInput = inSettings
      }

instance FromJSON FixedBoneConfig where
  parseJSON = withObject "FixedBoneConfig" $ \o -> do
    bone <- o .: "bone"
    pos <- o .: "position"
    rotMaybe <- o .:? "rotation"
    pure FixedBoneConfig
      { fixedBone = bone
      , fixedPosition = parseV3 pos
      , fixedRotation = parseQuatMaybe rotMaybe
      }

instance FromJSON EffectorConfig where
  parseJSON = withObject "EffectorConfig" $ \o -> do
    bone <- o .: "bone"
    keyframes <- o .: "keyframes"
    pure EffectorConfig
      { effectorBone = bone
      , effectorKeyframes = keyframes
      }

instance FromJSON KeyframeConfig where
  parseJSON = withObject "KeyframeConfig" $ \o -> do
    time <- o .: "time"
    pos <- o .: "position"
    rotMaybe <- o .:? "rotation"
    easingText <- o .:? "easing" .!= "linear"
    let easing = maybe Linear id (parseEasing easingText)
    pure KeyframeConfig
      { kfTime = time
      , kfPosition = parseV3 pos
      , kfRotation = parseQuatMaybe rotMaybe
      , kfEasing = easing
      }

instance FromJSON GenerationSettings where
  parseJSON = withObject "GenerationSettings" $ \o -> do
    frameRate <- o .:? "frameRate" .!= 30
    frameCount <- o .:? "frameCount"
    solver <- o .:? "solver" .!= "fabrik"
    loop <- o .:? "loop" .!= "once"
    rootMotion <- o .:? "rootMotion" .!= "fixed"
    pure GenerationSettings
      { settingsFrameRate = frameRate
      , settingsFrameCount = frameCount
      , settingsSolver = solver
      , settingsLoop = loop
      , settingsRootMotion = rootMotion
      }

instance FromJSON SkeletonSettings where
  parseJSON = withObject "SkeletonSettings" $ \o -> do
    detailText <- o .:? "detail" .!= "standard"
    boneLengths <- o .:? "boneLengths" .!= mempty
    let detail = parseSkeletonDetail detailText
    pure SkeletonSettings
      { skelDetail = detail
      , skelBoneLengths = boneLengths
      }

instance FromJSON OutputSettings where
  parseJSON = withObject "OutputSettings" $ \o -> do
    format <- o .:? "format" .!= "glb"
    optimize <- o .:? "optimize" .!= True
    precision <- o .:? "precision" .!= 6
    pure OutputSettings
      { outFormat = format
      , outOptimize = optimize
      , outPrecision = precision
      }

instance FromJSON InputSettings where
  parseJSON = withObject "InputSettings" $ \o -> do
    coordSys <- o .:? "coordinateSystem" .!= "y-up"
    pure InputSettings
      { inCoordinateSystem = coordSys
      }

-- Helper parsers

parseV3 :: [Float] -> V3 Float
parseV3 [x, y, z] = V3 x y z
parseV3 [x, y] = V3 x y 0
parseV3 [x] = V3 x 0 0
parseV3 _ = V3 0 0 0

parseQuatMaybe :: Maybe [Float] -> Maybe (Quaternion Float)
parseQuatMaybe Nothing = Nothing
parseQuatMaybe (Just [x, y, z, w]) = Just $ Quaternion w (V3 x y z)
parseQuatMaybe (Just _) = Nothing

parseSkeletonDetail :: Text -> SkeletonDetail
parseSkeletonDetail txt = case T.toLower txt of
  "minimal" -> MinimalSkeleton
  "standard" -> StandardSkeleton
  "full" -> FullSkeleton
  _ -> StandardSkeleton

-- | Load configuration from file path
loadConfigFromFile :: FilePath -> IO (Result AnimationConfig)
loadConfigFromFile path = do
  result <- Yaml.decodeFileEither path
  pure $ case result of
    Left err -> failure $ ConfigParseError path (show err)
    Right config -> success config

-- | Load configuration from ByteString
loadConfig :: ByteString -> Result AnimationConfig
loadConfig bs = case Yaml.decodeEither' bs of
  Left err -> failure $ ConfigParseError "<input>" (show err)
  Right config -> success config

-- | Parse configuration (alias for loadConfig)
parseConfig :: ByteString -> Result AnimationConfig
parseConfig = loadConfig

-- | Simplified configuration options (used by Blender import)
data ConfigOptions = ConfigOptions
  { optionsLoop :: Maybe String
  , optionsFps :: Maybe Float
  , optionsFrames :: Maybe Int
  , optionsSolver :: Maybe String
  , optionsOptimize :: Maybe Bool
  } deriving stock (Show, Eq)

-- ToJSON instances for writing YAML

instance ToJSON AnimationConfig where
  toJSON cfg = object
    [ "name" .= configName cfg
    , "duration" .= configDuration cfg
    , "fixed" .= configFixed cfg
    , "effector" .= configEffector cfg
    , "config" .= configSettings cfg
    ]

instance ToJSON FixedBoneConfig where
  toJSON cfg = object $
    [ "bone" .= fixedBone cfg
    , "position" .= v3ToList (fixedPosition cfg)
    ] ++ rotField
    where
      rotField = case fixedRotation cfg of
        Just q -> ["rotation" .= quatToList q]
        Nothing -> []

instance ToJSON EffectorConfig where
  toJSON cfg = object
    [ "bone" .= effectorBone cfg
    , "keyframes" .= effectorKeyframes cfg
    ]

instance ToJSON KeyframeConfig where
  toJSON cfg = object $
    [ "time" .= kfTime cfg
    , "position" .= v3ToList (kfPosition cfg)
    ] ++ rotField ++ easingField
    where
      rotField = case kfRotation cfg of
        Just q -> ["rotation" .= quatToList q]
        Nothing -> []
      easingField = case kfEasing cfg of
        Linear -> []
        e -> ["easing" .= easingToText e]

instance ToJSON GenerationSettings where
  toJSON cfg = object
    [ "frameRate" .= settingsFrameRate cfg
    , "solver" .= settingsSolver cfg
    , "loop" .= settingsLoop cfg
    ]

-- Helper functions for ToJSON

v3ToList :: V3 Float -> [Float]
v3ToList (V3 x y z) = [x, y, z]

quatToList :: Quaternion Float -> [Float]
quatToList (Quaternion w (V3 x y z)) = [x, y, z, w]

easingToText :: Easing -> Text
easingToText Linear = "linear"
easingToText EaseIn = "easeIn"
easingToText EaseOut = "easeOut"
easingToText EaseInOut = "easeInOut"
easingToText Cubic = "cubic"
easingToText CubicIn = "cubicIn"
easingToText CubicOut = "cubicOut"
easingToText QuartIn = "quartIn"
easingToText QuartOut = "quartOut"
easingToText QuartInOut = "quartInOut"
easingToText SineIn = "sineIn"
easingToText SineOut = "sineOut"
easingToText SineInOut = "sineInOut"
easingToText ExpoIn = "expoIn"
easingToText ExpoOut = "expoOut"
easingToText ExpoInOut = "expoInOut"
easingToText Bounce = "bounce"
easingToText (Custom _) = "custom"

-- | Write configuration to YAML file
writeConfigToFile :: FilePath -> AnimationConfig -> IO ()
writeConfigToFile path cfg = Yaml.encodeFile path cfg
