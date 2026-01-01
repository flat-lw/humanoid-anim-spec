-- |
-- Module      : HumanoidAnim
-- Description : Humanoid Animation Generator public API
--
-- This is the main entry point for the humanoid animation library.
-- It re-exports the most commonly used types and functions.
module HumanoidAnim
  ( -- * Animation Generation
    generateFromConfig
  , generateFromFile

    -- * Core Types
  , AnimationClip(..)
  , AnimationFrame(..)
  , LoopMode(..)
  , Transform(..)

    -- * Configuration
  , AnimationConfig(..)
  , loadConfigFromFile
  , validateConfig

    -- * Skeleton
  , Skeleton
  , SkeletonConfig(..)
  , SkeletonDetail(..)
  , buildSkeleton
  , defaultSkeletonConfig

    -- * Bones
  , HumanoidBone(..)
  , boneName
  , parseBoneName
  , boneCategory
  , bonesForDetail
  , boneParent

    -- * IK
  , IKConstraint(..)
  , FABRIK(..)
  , FABRIKConfig(..)
  , defaultFABRIKConfig
  , solveCCD
  , CCDConfig(..)
  , defaultCCDConfig

    -- * Blender Import
  , BlenderAnimation(..)
  , loadBlenderJson
  , blenderToConfig
  , blenderAnimName
  , blenderAnimFrameStart
  , blenderAnimFrameEnd
  , blenderAnimFps
  , writeConfigToFile

    -- * Output (GLTF)
  , GLTFOptions(..)
  , GLTFFormat(..)
  , exportGLTF
  , writeGLTF
  , defaultGLTFOptions

    -- * Output (Unity .anim)
  , UnityAnimOptions(..)
  , WrapMode(..)
  , exportUnityAnim
  , writeUnityAnim
  , defaultUnityAnimOptions

    -- * Error Handling
  , Result(..)
  , AppError(..)
  , AppWarning(..)
  , formatError
  , formatWarning

    -- * Re-exports
  , V3(..)
  , Quaternion(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Animation
import HumanoidAnim.Error
import HumanoidAnim.IK.Core
import HumanoidAnim.IK.FABRIK
import HumanoidAnim.IK.CCD
import HumanoidAnim.Input.Config
import HumanoidAnim.Input.Validation
import HumanoidAnim.Input.Blender hiding (boneName)
import HumanoidAnim.Motion.Keyframe (Keyframe(..))
import HumanoidAnim.Output.GLTF
import HumanoidAnim.Output.UnityAnim
import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config
import HumanoidAnim.Skeleton.Hierarchy (boneParent)

-- | Generate animation from a configuration
generateFromConfig :: AnimationConfig -> Result AnimationClip
generateFromConfig config = do
  -- Validate configuration
  validConfig <- validateConfig config

  -- Build skeleton
  let skelConfig = SkeletonConfig
        { skeletonDetail = skelDetail (configSkeleton config)
        , customBoneLengths = Map.empty
        , enabledBones = Nothing
        }
      skeleton = buildSkeleton skelConfig

  -- Get constraints
  constraints <- configToConstraints validConfig

  -- Get keyframes
  keyframes <- configToKeyframes validConfig

  -- Get effector bone
  effectorBone <- case parseBoneName (effectorBone (configEffector config)) of
    Just b -> success b
    Nothing -> failure $ InvalidBoneName (T.unpack $ effectorBone (configEffector config))

  -- Parse loop mode
  let loopMode = parseLoopMode (settingsLoop (configSettings config))

  -- Generate animation
  let genConfig = GenerationConfig
        { genFrameRate = settingsFrameRate (configSettings config)
        , genFrameCount = settingsFrameCount (configSettings config)
        , genSolverType = FABRIKSolver
        , genOptimize = outOptimize (configOutput config)
        }

  generateAnimation
    genConfig
    skeleton
    constraints
    effectorBone
    keyframes
    (T.unpack $ configName config)
    (configDuration config)
    loopMode

-- | Generate animation from a configuration file
generateFromFile :: FilePath -> IO (Result AnimationClip)
generateFromFile path = do
  configResult <- loadConfigFromFile path
  pure $ configResult >>= generateFromConfig

-- | Parse loop mode from text
parseLoopMode :: T.Text -> LoopMode
parseLoopMode txt = case T.toLower txt of
  "once" -> Once
  "cycle" -> Cycle
  "loop" -> Cycle
  "pingpong" -> PingPong
  "ping-pong" -> PingPong
  _ -> Once
