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

    -- * Spine IK Integration
  , applySpineIK
  , calculateSpineStiffness
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Error
import HumanoidAnim.IK.Core as IKCore
import HumanoidAnim.IK.TwoBone
import HumanoidAnim.IK.Spine
import HumanoidAnim.Motion.Keyframe (Keyframe(..))
import HumanoidAnim.Motion.Trajectory (keyframesToTrajectory)
import HumanoidAnim.Skeleton.Bones (HumanoidBone(..))
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Transform (computeRotations)
import HumanoidAnim.Skeleton.Hierarchy (getChainBetween)

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
  , clipFixedBones :: [(HumanoidBone, V3 Float)]
    -- ^ Fixed bones and their target positions (for RootT/RootQ calculation)
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
  , genSolverType = TwoBoneSolver
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

  -- Generate frames using the configured solver
  let solverType = genSolverType config
  frames <- mapM (generateFrame solverType skeleton fixedConstraints effector trajectory) frameTimes

  -- Collect warnings from IK solving
  let allWarnings = concatMap extractWarnings frames

  -- Extract fixed bone positions from constraints
  let fixedBones = [(bone, pos) | IKCore.Fixed bone pos <- fixedConstraints]
                ++ [(bone, pos) | IKCore.FixedWithRotation bone pos _ <- fixedConstraints]

  -- Build clip
  let clip = AnimationClip
        { clipName = name
        , clipDuration = duration
        , clipFrameRate = genFrameRate config
        , clipFrames = map fst frames
        , clipLoopMode = loopMode
        , clipFixedBones = fixedBones
        }

  addWarnings allWarnings (success clip)

-- | Generate a single animation frame
generateFrame
  :: SolverType
  -> Skeleton
  -> [IKConstraint]
  -> HumanoidBone
  -> (Float -> V3 Float)  -- ^ Trajectory function
  -> Float                -- ^ Time
  -> Result (AnimationFrame, [AppWarning])
generateFrame solverType skeleton fixedConstraints effector trajectory time = do
  let targetPos = trajectory time

  -- Initial pose from rest pose
  let initialPose = Map.map transformPosition (skeletonRestPose skeleton)

  -- Step 1: Apply Spine IK with continuous stiffness
  -- Only for hand effectors (arms), not for feet
  let isArmEffector = effector `elem` [RightHand, LeftHand]

      -- Calculate continuous stiffness based on reach distance
      -- Stiffness increases smoothly from 0 at 80% reach to 0.6 at 100%+ reach
      spineStiffness =
        if isArmEffector
        then calculateSpineStiffness skeleton initialPose effector targetPos
               0.80   -- Lower threshold: start blending at 80% reach
               1.00   -- Upper threshold: full stiffness at 100% reach
               0.6    -- Maximum stiffness
        else 0.0

      -- Apply Spine IK with the calculated stiffness
      -- TODO: Spine IK is currently disabled due to discontinuity issues
      -- The issue is that Spine IK rotation can move the shoulder in unexpected ways,
      -- causing discontinuities in the Two-Bone IK results.
      -- Future fix: Need to ensure Spine IK properly moves the shoulder toward the target.
      poseAfterSpine = initialPose
      _unusedSpineIK =
        if isArmEffector
        then
          let fixedBone = getSpineFixedBone fixedConstraints
              fixedPos = getFixedBonePosition fixedConstraints fixedBone initialPose
              spineTipBone = case effector of
                RightHand -> RightShoulder
                LeftHand -> LeftShoulder
                _ -> Neck
          in applySpineIK initialPose fixedBone fixedPos spineTipBone targetPos spineStiffness
        else initialPose

  -- Step 2: Solve limb IK (Two-Bone IK)
  let allConstraints = fixedConstraints ++ [Effector effector targetPos]
      ikInput = IKInput
        { ikSkeleton = skeleton
        , ikInitialPose = poseAfterSpine
        , ikConstraints = allConstraints
        }

  -- Solve IK using the configured solver
  let ikOutput = case solverType of
        TwoBoneSolver -> solveTwoBone defaultTwoBoneConfig ikInput
        CCDSolver     -> solveTwoBone defaultTwoBoneConfig ikInput  -- Fallback to TwoBone

  -- Convert IK warnings to app warnings
  let warnings = map convertIKWarning (ikWarnings ikOutput)

  -- Compute rotations from solved positions
  let solvedPose = ikResultPose ikOutput
      computedPose = computeRotations skeleton solvedPose Map.empty

  -- Apply fixed rotations from FixedWithRotation constraints
  let fixedRotations = extractFixedRotations fixedConstraints
      fullPose = applyFixedRotations fixedRotations computedPose

  let frame = AnimationFrame
        { frameTime = time
        , framePose = fullPose
        }

  success (frame, warnings)

-- | Get the fixed bone for spine IK from constraints
-- Prefers Hips, falls back to Chest, then defaults to Hips
getSpineFixedBone :: [IKConstraint] -> HumanoidBone
getSpineFixedBone constraints =
  let fixedBones = [bone | Fixed bone _ <- constraints]
                ++ [bone | FixedWithRotation bone _ _ <- constraints]
  in if Hips `elem` fixedBones then Hips
     else if Chest `elem` fixedBones then Chest
     else Hips  -- Default

-- | Get position of a fixed bone from constraints or initial pose
getFixedBonePosition :: [IKConstraint] -> HumanoidBone -> Map HumanoidBone (V3 Float) -> V3 Float
getFixedBonePosition constraints bone initialPose =
  case [pos | Fixed b pos <- constraints, b == bone] of
    (pos:_) -> pos
    [] -> case [pos | FixedWithRotation b pos _ <- constraints, b == bone] of
            (pos:_) -> pos
            [] -> Map.findWithDefault (V3 0 1 0) bone initialPose

-- | Extract fixed rotations from constraints
extractFixedRotations :: [IKConstraint] -> Map HumanoidBone (Quaternion Float)
extractFixedRotations constraints =
  Map.fromList [(bone, rot) | FixedWithRotation bone _ rot <- constraints]

-- | Apply fixed rotations to a pose
applyFixedRotations
  :: Map HumanoidBone (Quaternion Float)
  -> Map HumanoidBone Transform
  -> Map HumanoidBone Transform
applyFixedRotations fixedRots pose =
  Map.mapWithKey applyIfFixed pose
  where
    applyIfFixed bone transform =
      case Map.lookup bone fixedRots of
        Just rot -> transform { transformRotation = rot }
        Nothing -> transform

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

-- | Apply spine IK to a pose
--
-- This function applies spine IK to bend the spine chain toward a goal,
-- distributing rotation across bones between fixedBone and targetBone.
--
-- Example:
--   applySpineIK pose Hips hipsPos Head goalPos 0.8
--   -> Bends Spine/Chest/UpperChest/Neck toward goal
--
applySpineIK
  :: Map HumanoidBone (V3 Float)  -- ^ Current pose
  -> HumanoidBone                 -- ^ Fixed bone (anchor)
  -> V3 Float                     -- ^ Fixed bone position
  -> HumanoidBone                 -- ^ Target bone to move toward goal
  -> V3 Float                     -- ^ Goal position
  -> Float                        -- ^ Stiffness (0.0-1.0)
  -> Map HumanoidBone (V3 Float)  -- ^ Updated pose
applySpineIK pose fixedBone fixedPos targetBone goalPos stiffness =
  let -- Get weights for the chain between fixed and target
      weights = getSpineChainWeights fixedBone targetBone defaultSpineWeights
      config = SpineIKConfig { sicStiffness = stiffness }
      result = solveSpineIK pose fixedBone fixedPos targetBone goalPos weights config
  in sirPose result

-- | Calculate spine IK stiffness based on reach distance
--
-- Returns a continuous stiffness value (0.0-1.0) based on how far
-- the target is relative to the limb's maximum reach.
--
-- The stiffness transitions smoothly:
--   0% - 80% of max reach  → stiffness = 0 (no spine IK)
--   80% - 100% of max reach → stiffness = 0 → maxStiffness (smoothstep)
--   100%+ of max reach     → stiffness = maxStiffness
--
-- This ensures continuous motion without sudden jumps when
-- crossing the reach threshold.
--
calculateSpineStiffness
  :: Skeleton
  -> Map HumanoidBone (V3 Float)  -- ^ Current pose
  -> HumanoidBone                 -- ^ Effector (hand or foot)
  -> V3 Float                     -- ^ Target position
  -> Float                        -- ^ Lower threshold (e.g., 0.8 = 80%)
  -> Float                        -- ^ Upper threshold (e.g., 1.0 = 100%)
  -> Float                        -- ^ Maximum stiffness (e.g., 0.5)
  -> Float                        -- ^ Calculated stiffness (0.0 to maxStiffness)
calculateSpineStiffness skeleton pose effector targetPos lowerThresh upperThresh maxStiffness =
  let -- Get the root of the limb chain
      limbRoot = case effector of
        RightHand -> RightUpperArm
        LeftHand -> LeftUpperArm
        RightFoot -> RightUpperLeg
        LeftFoot -> LeftUpperLeg
        _ -> effector

      -- Get limb root position
      rootPos = fromMaybe (V3 0 1 0) $ Map.lookup limbRoot pose

      -- Calculate max reach (sum of bone lengths in the chain)
      lengths = skeletonLengths skeleton
      maxReach = case effector of
        RightHand -> (fromMaybe 0.3 $ Map.lookup RightUpperArm lengths)
                   + (fromMaybe 0.25 $ Map.lookup RightLowerArm lengths)
        LeftHand -> (fromMaybe 0.3 $ Map.lookup LeftUpperArm lengths)
                   + (fromMaybe 0.25 $ Map.lookup LeftLowerArm lengths)
        RightFoot -> (fromMaybe 0.4 $ Map.lookup RightUpperLeg lengths)
                   + (fromMaybe 0.4 $ Map.lookup RightLowerLeg lengths)
        LeftFoot -> (fromMaybe 0.4 $ Map.lookup LeftUpperLeg lengths)
                   + (fromMaybe 0.4 $ Map.lookup LeftLowerLeg lengths)
        _ -> 0.5

      -- Distance to target as fraction of max reach
      distToTarget = normV3 (targetPos `subV3` rootPos)
      reachFraction = if maxReach > 0.0001 then distToTarget / maxReach else 0

      -- Smoothstep interpolation between thresholds
      t = smoothstepF lowerThresh upperThresh reachFraction

  in t * maxStiffness
  where
    normV3 (V3 x y z) = sqrt (x*x + y*y + z*z)
    subV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1-x2) (y1-y2) (z1-z2)
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

-- | Smoothstep function for continuous interpolation
smoothstepF :: Float -> Float -> Float -> Float
smoothstepF edge0 edge1 x
  | x <= edge0 = 0
  | x >= edge1 = 1
  | otherwise  = let t = (x - edge0) / (edge1 - edge0)
                 in t * t * (3 - 2 * t)

-- | Blend two poses based on a blend factor (0.0 = pose1, 1.0 = pose2)
blendPoses
  :: Map HumanoidBone (V3 Float)  -- ^ Pose 1 (blend factor 0)
  -> Map HumanoidBone (V3 Float)  -- ^ Pose 2 (blend factor 1)
  -> Float                        -- ^ Blend factor (0.0-1.0)
  -> Map HumanoidBone (V3 Float)
blendPoses pose1 pose2 t =
  Map.mapWithKey blendBone pose1
  where
    blendBone bone pos1 =
      case Map.lookup bone pose2 of
        Just pos2 -> lerpV3 pos1 pos2 t
        Nothing -> pos1
    lerpV3 (V3 x1 y1 z1) (V3 x2 y2 z2) s =
      V3 (x1 + (x2 - x1) * s)
         (y1 + (y2 - y1) * s)
         (z1 + (z2 - z1) * s)
