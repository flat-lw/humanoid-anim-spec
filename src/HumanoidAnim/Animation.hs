-- |
-- Module      : HumanoidAnim.Animation
-- Description : Animation generation
--
-- This module handles the main animation generation pipeline, combining
-- IK solving with keyframe interpolation to produce animation clips.
module HumanoidAnim.Animation
  ( -- * Animation Types
    LoopMode(..)
  , AnimationFrame(..)
  , AnimationClip(..)
  , GenerationConfig(..)

    -- * Generation
  , generateAnimation
  , defaultGenerationConfig

    -- * Frame Operations
  , sampleAtTime
  , getFrameTimes
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Error
import HumanoidAnim.IK.Core as IKCore
import HumanoidAnim.IK.FABRIK
import HumanoidAnim.Motion.Keyframe (Keyframe(..))
import HumanoidAnim.Motion.Trajectory (keyframesToTrajectory)
import HumanoidAnim.Skeleton.Bones (HumanoidBone)
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Transform (computeRotations)

-- | Loop mode for animation playback
data LoopMode
  = Once      -- ^ Play once and stop
  | Cycle     -- ^ Loop continuously
  | PingPong  -- ^ Play forward, then backward, repeat
  deriving stock (Show, Eq, Read, Enum, Bounded)

-- | A single animation frame
data AnimationFrame = AnimationFrame
  { frameTime :: Float
    -- ^ Time in seconds
  , framePose :: Map HumanoidBone Transform
    -- ^ Full pose with positions and rotations
  } deriving stock (Show, Eq)

-- | Complete animation clip
data AnimationClip = AnimationClip
  { clipName :: String
    -- ^ Animation name
  , clipDuration :: Float
    -- ^ Total duration in seconds
  , clipFrameRate :: Float
    -- ^ Frame rate (fps)
  , clipFrames :: [AnimationFrame]
    -- ^ List of frames
  , clipLoopMode :: LoopMode
    -- ^ Loop mode
  } deriving stock (Show, Eq)

-- | Configuration for animation generation
data GenerationConfig = GenerationConfig
  { genFrameRate :: Float
    -- ^ Frame rate (fps)
  , genFrameCount :: Maybe Int
    -- ^ Optional fixed frame count
  , genSolverType :: SolverType
    -- ^ IK solver type to use
  , genOptimize :: Bool
    -- ^ Whether to optimize keyframes
  } deriving stock (Show, Eq)

-- | Default generation configuration
defaultGenerationConfig :: GenerationConfig
defaultGenerationConfig = GenerationConfig
  { genFrameRate = 30
  , genFrameCount = Nothing
  , genSolverType = FABRIKSolver
  , genOptimize = True
  }

-- | Generate animation from constraints and keyframes
generateAnimation
  :: GenerationConfig
  -> Skeleton
  -> [IKConstraint]      -- ^ Fixed constraints
  -> HumanoidBone        -- ^ Effector bone
  -> [Keyframe]          -- ^ Effector trajectory keyframes
  -> String              -- ^ Animation name
  -> Float               -- ^ Duration
  -> LoopMode
  -> Result AnimationClip
generateAnimation config skeleton fixedConstraints effector keyframes name duration loopMode = do
  -- Validate inputs
  when (null keyframes) $
    failure $ InsufficientKeyframes 0

  -- Create trajectory from keyframes
  let trajectory = keyframesToTrajectory keyframes

  -- Get frame times
  let frameTimes = getFrameTimes (genFrameRate config) (genFrameCount config) duration

  -- Generate frames
  frames <- mapM (generateFrame skeleton fixedConstraints effector trajectory) frameTimes

  -- Collect warnings from IK solving
  let allWarnings = concatMap extractWarnings frames

  -- Build clip
  let clip = AnimationClip
        { clipName = name
        , clipDuration = duration
        , clipFrameRate = genFrameRate config
        , clipFrames = map fst frames
        , clipLoopMode = loopMode
        }

  addWarnings allWarnings (success clip)

-- | Generate a single animation frame
generateFrame
  :: Skeleton
  -> [IKConstraint]
  -> HumanoidBone
  -> (Float -> V3 Float)  -- ^ Trajectory function
  -> Float                -- ^ Time
  -> Result (AnimationFrame, [AppWarning])
generateFrame skeleton fixedConstraints effector trajectory time = do
  let targetPos = trajectory time

  -- Create IK input with effector constraint
  let allConstraints = fixedConstraints ++ [Effector effector targetPos]
      initialPose = Map.map transformPosition (skeletonRestPose skeleton)
      ikInput = IKInput
        { ikSkeleton = skeleton
        , ikInitialPose = initialPose
        , ikConstraints = allConstraints
        }

  -- Solve IK
  let fabrikConfig = FABRIKConfig 50 0.001
      ikOutput = solveFABRIK fabrikConfig ikInput

  -- Convert IK warnings to app warnings
  let warnings = map convertIKWarning (ikWarnings ikOutput)

  -- Compute rotations from solved positions
  let solvedPose = ikResultPose ikOutput
      fullPose = computeRotations skeleton solvedPose Map.empty

  let frame = AnimationFrame
        { frameTime = time
        , framePose = fullPose
        }

  success (frame, warnings)

-- | Convert IK warning to app warning
convertIKWarning :: IKWarning -> AppWarning
convertIKWarning (IKCore.UnreachableTarget bone dist) = HumanoidAnim.Error.UnreachableTarget bone dist
convertIKWarning (IKCore.DidNotConverge iters err) = IKNotConverged iters err

-- | Extract warnings from frame generation result
extractWarnings :: (AnimationFrame, [AppWarning]) -> [AppWarning]
extractWarnings (_, ws) = ws

-- | Sample animation at a specific time
sampleAtTime :: AnimationClip -> Float -> Maybe AnimationFrame
sampleAtTime clip time
  | null (clipFrames clip) = Nothing
  | time <= 0 = Just (head (clipFrames clip))
  | time >= clipDuration clip = Just (last (clipFrames clip))
  | otherwise = findClosestFrame (clipFrames clip) time

-- | Find the closest frame to a given time
findClosestFrame :: [AnimationFrame] -> Float -> Maybe AnimationFrame
findClosestFrame [] _ = Nothing
findClosestFrame [f] _ = Just f
findClosestFrame (f1:f2:rest) time
  | time < frameTime f2 = Just f1
  | otherwise = findClosestFrame (f2:rest) time

-- | Get frame times for animation
getFrameTimes :: Float -> Maybe Int -> Float -> [Float]
getFrameTimes fps maybeCount duration =
  case maybeCount of
    Just count -> [duration * fromIntegral i / fromIntegral (count - 1) | i <- [0..count-1]]
    Nothing ->
      let ft = 1 / fps
          numFrames = ceiling (duration * fps) :: Int
      in [fromIntegral i * ft | i <- [0..numFrames]]

-- Helper for when in Result monad
when :: Bool -> Result () -> Result ()
when True action = action
when False _ = success ()
