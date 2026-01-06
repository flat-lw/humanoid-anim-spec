-- |
-- Module      : HumanoidAnim.IK.TwoBone
-- Description : Two-Bone IK solver
--
-- Implementation of the Two-Bone IK algorithm for inverse kinematics.
-- This is the standard approach used for arms (shoulder-elbow-hand) and
-- legs (hip-knee-foot) in most game engines including Unity and Unreal.
--
-- The algorithm uses the law of cosines to find the elbow/knee position
-- given the shoulder/hip position, target hand/foot position, and bone lengths.
--
-- This module supports both Forward IK (move end effector) and Inverse IK
-- (fix end effector, move root) for flexible animation scenarios like:
--   - Squats (feet fixed, hips move)
--   - Push-ups (hands fixed, body moves)
--   - Wall pushing (hand fixed, shoulder moves)
module HumanoidAnim.IK.TwoBone
  ( -- * Solver Type
    TwoBone(..)
  , TwoBoneConfig(..)

    -- * Limb Chain Types
  , LimbChain(..)
  , IKDirection(..)
  , allLimbChains

    -- * Solving Functions
  , solveTwoBone
  , defaultTwoBoneConfig
  , solveTwoBoneChain
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find)
import Linear (V3(..), norm, normalize, cross, dot, (^*), (^-^), (^+^))

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.IK.Core

-- | Two-Bone IK solver singleton type
data TwoBone = TwoBone
  deriving stock (Show, Eq)

-- | Two-Bone IK configuration
data TwoBoneConfig = TwoBoneConfig
  { twoBoneTolerance :: Float
    -- ^ Convergence tolerance in meters (default: 0.001)
  } deriving stock (Show, Eq)

-- | Default Two-Bone IK configuration
defaultTwoBoneConfig :: TwoBoneConfig
defaultTwoBoneConfig = TwoBoneConfig
  { twoBoneTolerance = 0.001
  }

-- | IK direction for a limb chain
data IKDirection
  = ForwardIK   -- ^ Root fixed, move end effector (normal IK)
  | InverseIK   -- ^ End fixed, compute mid from root position
  deriving stock (Show, Eq)

-- | A limb chain defines the bones in a two-bone IK chain
--
-- Each limb has:
--   - Root: The upper bone (UpperArm/UpperLeg) that connects to the body
--   - Mid: The middle joint (LowerArm/LowerLeg)
--   - End: The end effector (Hand/Foot)
--   - Parent: The bone that the root attaches to (Shoulder/Hips)
data LimbChain = LimbChain
  { limbRoot   :: HumanoidBone  -- ^ Upper bone (UpperArm/UpperLeg)
  , limbMid    :: HumanoidBone  -- ^ Middle joint (LowerArm/LowerLeg)
  , limbEnd    :: HumanoidBone  -- ^ End effector (Hand/Foot)
  , limbParent :: HumanoidBone  -- ^ Parent bone (Shoulder/Hips)
  } deriving stock (Show, Eq)

-- | All limb chains in the humanoid skeleton
allLimbChains :: [LimbChain]
allLimbChains =
  [ -- Right arm: Shoulder -> UpperArm -> LowerArm -> Hand
    LimbChain RightUpperArm RightLowerArm RightHand RightShoulder
    -- Left arm
  , LimbChain LeftUpperArm LeftLowerArm LeftHand LeftShoulder
    -- Right leg: Hips -> UpperLeg -> LowerLeg -> Foot
  , LimbChain RightUpperLeg RightLowerLeg RightFoot Hips
    -- Left leg
  , LimbChain LeftUpperLeg LeftLowerLeg LeftFoot Hips
  ]

-- | Find the limb chain that contains a given bone as its end effector
findChainByEnd :: HumanoidBone -> Maybe LimbChain
findChainByEnd bone = find (\c -> limbEnd c == bone) allLimbChains

-- | Find the limb chain that contains a given bone as its root
findChainByRoot :: HumanoidBone -> Maybe LimbChain
findChainByRoot bone = find (\c -> limbRoot c == bone) allLimbChains

-- | Check if a bone is a limb end effector (Hand/Foot)
isLimbEnd :: HumanoidBone -> Bool
isLimbEnd bone = bone `elem` [RightHand, LeftHand, RightFoot, LeftFoot]

-- | Check if a bone is a limb root (UpperArm/UpperLeg)
isLimbRoot :: HumanoidBone -> Bool
isLimbRoot bone = bone `elem` [RightUpperArm, LeftUpperArm, RightUpperLeg, LeftUpperLeg]

-- | Get the offset from parent to root bone in rest pose
--
-- This is used to compute the root position when the parent moves.
-- For example, when Hips move, we need to compute where UpperLeg goes.
getRootOffsetFromParent :: Skeleton -> LimbChain -> V3 Float
getRootOffsetFromParent skeleton chain =
  let restPose = skeletonRestPose skeleton
      parentPos = maybe (V3 0 0 0) transformPosition $ Map.lookup (limbParent chain) restPose
      rootPos = maybe (V3 0 0 0) transformPosition $ Map.lookup (limbRoot chain) restPose
  in rootPos ^-^ parentPos

-- | Main Two-Bone IK solver entry point
solveTwoBone :: TwoBoneConfig -> IKInput -> IKOutput
solveTwoBone cfg input = IKOutput
  { ikResultPose = finalPose
  , ikIterations = 1  -- Two-Bone IK is analytical, always 1 iteration
  , ikConverged = converged
  , ikError = finalError
  , ikWarnings = warnings
  }
  where
    skeleton = ikSkeleton input
    initialPose = ikInitialPose input
    constraints = ikConstraints input

    -- Separate fixed constraints and effector constraints
    fixedConstraints = [c | c@(Fixed _ _) <- constraints] ++
                       [c | c@(FixedWithRotation _ _ _) <- constraints]
    effectorConstraints = [c | c@(Effector _ _) <- constraints] ++
                          [Effector b p | EffectorWithRotation b p _ <- constraints]

    -- Solve using the new unified approach
    (finalPose, converged, finalError) =
      solveAllConstraints cfg skeleton initialPose fixedConstraints effectorConstraints

    -- Generate warnings
    warnings = if converged then [] else [DidNotConverge 1 finalError]

-- | Determine IK direction for a limb chain based on constraints
--
-- Returns:
--   Just InverseIK: End is fixed AND parent/root moves (squat, push-up)
--   Just ForwardIK: End is the effector target (normal arm/leg IK)
--   Nothing: This chain is not affected by the constraints
determineIKDirection
  :: [IKConstraint]   -- ^ Fixed constraints
  -> [IKConstraint]   -- ^ Effector constraints
  -> LimbChain        -- ^ The limb chain to check
  -> Maybe IKDirection
determineIKDirection fixedCs effectorCs chain =
  let -- Check if the end effector (Hand/Foot) is fixed
      endFixed = any (\c -> constraintBone c == limbEnd chain) fixedCs

      -- Check if the parent (Hips/Shoulder) or root (UpperArm/UpperLeg) is an effector
      parentMoved = any (\c -> constraintBone c == limbParent chain) effectorCs
      rootMoved = any (\c -> constraintBone c == limbRoot chain) effectorCs

      -- Check if the end is an effector (normal forward IK)
      endIsEffector = any (\c -> constraintBone c == limbEnd chain) effectorCs

  in case (endFixed, parentMoved || rootMoved, endIsEffector) of
       (True, True, _)      -> Just InverseIK   -- End fixed, parent moves
       (_, _, True)         -> Just ForwardIK   -- End is effector target
       (True, False, False) -> Nothing          -- End fixed but nothing moves
       _                    -> Nothing          -- No effect on this chain

-- | Solve all constraints (both fixed and effector)
--
-- This function handles:
-- 1. Inverse IK: When end effectors (hands/feet) are fixed and parents move
-- 2. Forward IK: When end effectors are targets to reach
solveAllConstraints
  :: TwoBoneConfig
  -> Skeleton
  -> Map HumanoidBone (V3 Float)  -- ^ Initial pose
  -> [IKConstraint]               -- ^ Fixed constraints
  -> [IKConstraint]               -- ^ Effector constraints
  -> (Map HumanoidBone (V3 Float), Bool, Float)
solveAllConstraints cfg skeleton pose fixedCs effectorCs =
  let
    -- Step 1: Apply effector positions directly to the pose
    -- This sets positions for bones like Hips that are directly controlled
    poseWithEffectors = applyEffectorPositions pose effectorCs

    -- Step 2: Determine which chains need Inverse IK
    chainDirections = mapMaybe
      (\chain -> fmap (chain,) (determineIKDirection fixedCs effectorCs chain))
      allLimbChains

    inverseChains = [(chain, pos) | (chain, InverseIK) <- chainDirections
                                  , Just pos <- [getFixedPosition fixedCs (limbEnd chain)]]
    forwardChains = [(chain, pos) | (chain, ForwardIK) <- chainDirections
                                  , Just pos <- [getEffectorPosition effectorCs (limbEnd chain)]]

    -- Step 3: Solve Inverse IK chains first
    -- These compute mid positions based on fixed end and moved root
    (poseAfterInverse, invConv, invErr) =
      solveInverseChains cfg skeleton poseWithEffectors fixedCs inverseChains

    -- Step 4: Solve Forward IK chains
    -- These are normal effector-based IK
    (finalPose, fwdConv, fwdErr) =
      solveForwardChains cfg skeleton poseAfterInverse forwardChains

  in (finalPose, invConv && fwdConv, max invErr fwdErr)

-- | Apply effector positions to the pose
applyEffectorPositions
  :: Map HumanoidBone (V3 Float)
  -> [IKConstraint]
  -> Map HumanoidBone (V3 Float)
applyEffectorPositions pose effectorCs =
  foldr applyOne pose effectorCs
  where
    applyOne (Effector bone pos) p = Map.insert bone pos p
    applyOne _ p = p

-- | Get the fixed position for a bone from fixed constraints
getFixedPosition :: [IKConstraint] -> HumanoidBone -> Maybe (V3 Float)
getFixedPosition fixedCs bone =
  case [pos | c <- fixedCs, let (b, pos) = getFixedBonePos c, b == bone] of
    (pos:_) -> Just pos
    []      -> Nothing
  where
    getFixedBonePos (Fixed b p) = (b, p)
    getFixedBonePos (FixedWithRotation b p _) = (b, p)
    getFixedBonePos _ = (bone, V3 0 0 0)  -- dummy, won't match

-- | Get the effector position for a bone from effector constraints
getEffectorPosition :: [IKConstraint] -> HumanoidBone -> Maybe (V3 Float)
getEffectorPosition effectorCs bone =
  case [pos | Effector b pos <- effectorCs, b == bone] of
    (pos:_) -> Just pos
    []      -> Nothing

-- | Solve multiple Inverse IK chains
--
-- Inverse IK: Given a fixed end position and a moved parent,
-- compute the root and mid positions.
solveInverseChains
  :: TwoBoneConfig
  -> Skeleton
  -> Map HumanoidBone (V3 Float)
  -> [IKConstraint]                    -- ^ Fixed constraints
  -> [(LimbChain, V3 Float)]           -- ^ (Chain, fixed end position)
  -> (Map HumanoidBone (V3 Float), Bool, Float)
solveInverseChains cfg skeleton pose _fixedCs chains =
  foldr solveOne (pose, True, 0) chains
  where
    boneLengthMap = skeletonLengths skeleton

    solveOne (chain, fixedEndPos) (currentPose, allConv, maxErr) =
      let
        -- Get the parent position (e.g., Hips) from the current pose
        parentPos = fromMaybe (V3 0 0 0) $ Map.lookup (limbParent chain) currentPose

        -- Compute root position from parent
        -- For legs: UpperLeg is offset from Hips
        -- For arms: UpperArm is offset from Shoulder
        rootOffset = getRootOffsetFromParent skeleton chain
        rootPos = parentPos ^+^ rootOffset

        -- Get bone lengths
        len1 = fromMaybe 0.1 $ Map.lookup (limbRoot chain) boneLengthMap
        len2 = fromMaybe 0.1 $ Map.lookup (limbMid chain) boneLengthMap

        -- Compute pole vector for natural joint bend
        poleVector = computeNaturalPoleVector (limbEnd chain) rootPos fixedEndPos

        -- Solve Two-Bone IK from root to fixed end
        (midPos, _actualEndPos, err) = solveTwoBoneChain rootPos fixedEndPos len1 len2 poleVector

        -- Update pose with computed positions
        -- Use fixedEndPos (not actualEndPos) to ensure the foot stays fixed
        newPose = Map.insert (limbRoot chain) rootPos $
                  Map.insert (limbMid chain) midPos $
                  Map.insert (limbEnd chain) fixedEndPos currentPose

        conv = err < twoBoneTolerance cfg

      in (newPose, allConv && conv, max maxErr err)

-- | Solve multiple Forward IK chains (normal IK)
solveForwardChains
  :: TwoBoneConfig
  -> Skeleton
  -> Map HumanoidBone (V3 Float)
  -> [(LimbChain, V3 Float)]           -- ^ (Chain, target end position)
  -> (Map HumanoidBone (V3 Float), Bool, Float)
solveForwardChains cfg skeleton pose chains =
  foldr solveOne (pose, True, 0) chains
  where
    boneLengthMap = skeletonLengths skeleton

    solveOne (chain, targetPos) (currentPose, allConv, maxErr) =
      let
        -- Get root position from pose
        rootPos = fromMaybe (V3 0 0 0) $ Map.lookup (limbRoot chain) currentPose

        -- Get bone lengths
        len1 = fromMaybe 0.1 $ Map.lookup (limbRoot chain) boneLengthMap
        midBoneLen = fromMaybe 0.1 $ Map.lookup (limbMid chain) boneLengthMap
        -- Add end bone length if targeting the end effector
        len2 = midBoneLen

        -- Clamp target to joint limits
        clampedTarget = clampTargetToJointLimits (limbEnd chain) rootPos targetPos (len1 + len2)

        -- Compute pole vector
        poleVector = computeNaturalPoleVector (limbEnd chain) rootPos clampedTarget

        -- Solve IK
        (midPos, endPos, err) = solveTwoBoneChain rootPos clampedTarget len1 len2 poleVector

        -- Update pose
        newPose = Map.insert (limbMid chain) midPos $
                  Map.insert (limbEnd chain) endPos currentPose

        conv = err < twoBoneTolerance cfg

      in (newPose, allConv && conv, max maxErr err)

-- | Legacy function for backward compatibility
-- Now delegates to the new constraint-based solver
solveAllEffectors
  :: TwoBoneConfig
  -> Skeleton
  -> Map HumanoidBone (V3 Float)
  -> [IKConstraint]
  -> (Map HumanoidBone (V3 Float), Bool, Float)
solveAllEffectors cfg skeleton pose effectorCs =
  solveAllConstraints cfg skeleton pose [] effectorCs

-- | Get the two-bone chain (root, mid, end) for an effector
--
-- Two-Bone IK chain structure:
--   root = fixed joint (shoulder/hip position)
--   mid  = bend joint (elbow/knee position) - this gets updated by IK
--   end  = effector position - this gets updated by IK
--
-- For arms: root=UpperArm (shoulder joint), mid=LowerArm (elbow), end=Hand
-- For legs: root=UpperLeg (hip joint), mid=LowerLeg (knee), end=Foot
getTwoBoneChain :: HumanoidBone -> (HumanoidBone, HumanoidBone, HumanoidBone)
getTwoBoneChain effector = case effector of
  -- Right arm: shoulder joint -> elbow -> hand
  RightHand     -> (RightUpperArm, RightLowerArm, RightHand)
  RightLowerArm -> (RightUpperArm, RightLowerArm, RightLowerArm)
  -- Left arm: shoulder joint -> elbow -> hand
  LeftHand      -> (LeftUpperArm, LeftLowerArm, LeftHand)
  LeftLowerArm  -> (LeftUpperArm, LeftLowerArm, LeftLowerArm)
  -- Right leg: hip joint -> knee -> foot
  RightFoot     -> (RightUpperLeg, RightLowerLeg, RightFoot)
  RightToes     -> (RightUpperLeg, RightLowerLeg, RightFoot)
  RightLowerLeg -> (RightUpperLeg, RightLowerLeg, RightLowerLeg)
  -- Left leg: hip joint -> knee -> foot
  LeftFoot      -> (LeftUpperLeg, LeftLowerLeg, LeftFoot)
  LeftToes      -> (LeftUpperLeg, LeftLowerLeg, LeftFoot)
  LeftLowerLeg  -> (LeftUpperLeg, LeftLowerLeg, LeftLowerLeg)
  -- Hips: special case, returns identity to trigger inverse leg IK
  Hips          -> (Hips, Hips, Hips)
  -- Default: use effector itself
  _ -> (effector, effector, effector)

-- | Compute natural pole vector based on target position
--
-- This function determines the elbow/knee bend direction based on
-- anatomically natural movements:
--
-- For arms:
--   - Hand above shoulder: elbow bends outward and slightly forward
--   - Hand in front: elbow bends outward and down
--   - Hand at side: elbow bends backward
--   - Hand below: elbow bends backward and outward
--
-- For legs:
--   - Knee always bends forward (anatomical constraint)
--
computeNaturalPoleVector :: HumanoidBone -> V3 Float -> V3 Float -> V3 Float
computeNaturalPoleVector effector rootPos targetPos =
  let toTarget = targetPos ^-^ rootPos
      targetDist = norm toTarget
      -- Normalize direction, with fallback for zero-length
      V3 dx dy dz = if targetDist < 0.0001
                    then V3 0 (-1) 0
                    else toTarget ^* (1 / targetDist)
  in case effector of
    -- Right arm: natural elbow positions based on hand location
    RightHand -> computeArmPoleVector dx dy dz True
    RightLowerArm -> computeArmPoleVector dx dy dz True

    -- Left arm: mirror of right arm
    LeftHand -> computeArmPoleVector dx dy dz False
    LeftLowerArm -> computeArmPoleVector dx dy dz False

    -- Legs: knee always bends forward
    RightFoot     -> V3 0 0 1
    RightToes     -> V3 0 0 1
    RightLowerLeg -> V3 0 0 1
    LeftFoot      -> V3 0 0 1
    LeftToes      -> V3 0 0 1
    LeftLowerLeg  -> V3 0 0 1

    -- Default
    _ -> V3 0 0 1

-- | Compute arm pole vector based on hand direction
--
-- Uses continuous interpolation to avoid discontinuities when hand
-- crosses threshold boundaries. Each direction (up, down, front, back)
-- contributes to the final pole vector based on smooth weights.
--
-- Anatomical intentions:
--   - Hand above shoulder: elbow forward and outward (banzai pose)
--   - Hand in front: elbow outward and down
--   - Hand behind: elbow outward
--   - Hand below shoulder: elbow backward and outward
--   - Default (hand at side): elbow backward
--
computeArmPoleVector :: Float -> Float -> Float -> Bool -> V3 Float
computeArmPoleVector _dx dy dz isRight =
  let -- Lateral direction (outward from body)
      -- Right arm: +X is outward, Left arm: -X is outward
      outward = if isRight then 1.0 else (-1.0)

      -- Base pole vectors for each direction
      poleUp    = V3 (outward * 0.3) 0 0.7       -- elbow forward/outward
      poleDown  = V3 (outward * 0.3) 0 (-0.7)   -- elbow backward/outward
      poleFront = V3 (outward * 0.5) (-0.5) 0   -- elbow outward/down
      poleBack  = V3 (outward * 0.7) 0 0.3      -- elbow outward
      poleDefault = V3 (outward * 0.2) 0 (-0.6) -- neutral position

      -- Compute smooth weights for each direction
      -- smoothstep provides smooth transition in the range [edge0, edge1]
      weightUp    = smoothstep 0.0 0.5 dy         -- hand going up
      weightDown  = smoothstep 0.0 0.5 (-dy)      -- hand going down
      weightFront = smoothstep 0.0 0.5 dz         -- hand going forward
      weightBack  = smoothstep 0.0 0.5 (-dz)      -- hand going backward

      -- Blend all pole vectors based on weights
      -- Default contributes when no strong directional signal
      totalWeight = weightUp + weightDown + weightFront + weightBack
      defaultWeight = max 0 (1.0 - totalWeight)

      blendedPole = poleUp ^* weightUp
                ^+^ poleDown ^* weightDown
                ^+^ poleFront ^* weightFront
                ^+^ poleBack ^* weightBack
                ^+^ poleDefault ^* defaultWeight

  in normalize blendedPole

-- | Smooth step function for continuous interpolation
-- Returns 0 when x <= edge0, 1 when x >= edge1, smooth transition between
smoothstep :: Float -> Float -> Float -> Float
smoothstep edge0 edge1 x
  | x <= edge0 = 0
  | x >= edge1 = 1
  | otherwise  = let t = (x - edge0) / (edge1 - edge0)
                 in t * t * (3 - 2 * t)

-- | Solve a two-bone IK chain using the law of cosines
--
-- Given:
--   - rootPos: position of the root joint (shoulder/hip)
--   - targetPos: desired position of the end effector (hand/foot)
--   - len1: length of first bone (upper arm/thigh)
--   - len2: length of second bone (forearm/shin)
--   - poleVector: hint for the bend direction (elbow/knee)
--
-- Returns:
--   - midPos: position of the middle joint (elbow/knee)
--   - endPos: position of the end effector (actual achieved position)
--   - error: distance from achieved position to target
--
solveTwoBoneChain
  :: V3 Float     -- ^ Root position (shoulder/hip)
  -> V3 Float     -- ^ Target position (desired hand/foot)
  -> Float        -- ^ Length of first bone (upper arm/thigh)
  -> Float        -- ^ Length of second bone (forearm/shin)
  -> V3 Float     -- ^ Pole vector (bend direction hint)
  -> (V3 Float, V3 Float, Float)  -- ^ (mid position, end position, error)
solveTwoBoneChain rootPos targetPos len1 len2 poleHint =
  let
    -- Vector from root to target
    toTarget = targetPos ^-^ rootPos
    targetDist = norm toTarget

    -- Total chain length
    totalLen = len1 + len2

    -- Clamp target distance to reachable range
    -- If too far, stretch to max; if too close, compress to min
    clampedDist = clamp (abs (len1 - len2) + 0.001) (totalLen - 0.001) targetDist

    -- Direction from root to target
    targetDir = if targetDist < 0.0001
                then V3 1 0 0  -- Default direction if target is at root
                else normalize toTarget

    -- Use law of cosines to find the angle at the root joint
    -- len1^2 + clampedDist^2 - len2^2 = 2 * len1 * clampedDist * cos(angle)
    cosAngle = (len1 * len1 + clampedDist * clampedDist - len2 * len2)
               / (2 * len1 * clampedDist)
    cosAngleClamped = clamp (-1) 1 cosAngle

    -- Distance along target direction to the mid joint projection
    adjacentLen = len1 * cosAngleClamped

    -- Height of the mid joint above the root-target line
    oppositeLen = len1 * sqrt (1 - cosAngleClamped * cosAngleClamped)

    -- Build orthonormal basis for the IK plane
    -- We need a vector perpendicular to targetDir in the direction of poleHint
    perpVector = calculatePerpVector targetDir poleHint

    -- Calculate mid joint position
    midPos = rootPos
           ^+^ (targetDir ^* adjacentLen)
           ^+^ (perpVector ^* oppositeLen)

    -- Calculate end effector position (at len2 from midPos toward target)
    toTargetFromMid = targetPos ^-^ midPos
    toTargetFromMidDist = norm toTargetFromMid
    endDir = if toTargetFromMidDist < 0.0001
             then targetDir
             else normalize toTargetFromMid
    endPos = midPos ^+^ (endDir ^* len2)

    -- Calculate error
    err = norm (endPos ^-^ targetPos)

  in (midPos, endPos, err)

-- | Calculate a vector perpendicular to the target direction,
-- in the general direction of the pole hint
calculatePerpVector :: V3 Float -> V3 Float -> V3 Float
calculatePerpVector targetDir poleHint =
  let
    -- Project pole hint onto the plane perpendicular to targetDir
    projected = poleHint ^-^ (targetDir ^* dot poleHint targetDir)
    projLen = norm projected
  in
    if projLen < 0.0001
      -- Pole hint is parallel to target direction, use a default perpendicular
      then let fallback = if abs (dot targetDir (V3 1 0 0)) < 0.9
                          then V3 1 0 0
                          else V3 0 1 0
               perpCandidate = cross targetDir fallback
           in normalize perpCandidate
      else normalize projected

-- | Clamp a value to a range
clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal val = max minVal (min maxVal val)

-- | Clamp target position to Humanoid joint limits
--
-- Unity Humanoid has angular limits for each joint. If the target position
-- would require the joint to rotate beyond its limits, we clamp the target
-- to the nearest position within the valid range.
--
-- Joint limits (in degrees from horizontal):
--   Arms: Down-Up = -60° to +100°, Front-Back = -100° to +60°
--   Legs: Front-Back = -90° to +50°, In-Out = -60° to +60°
clampTargetToJointLimits :: HumanoidBone -> V3 Float -> V3 Float -> Float -> V3 Float
clampTargetToJointLimits effector rootPos targetPos maxReach =
  let toTarget = targetPos ^-^ rootPos
      dist = norm toTarget

      -- Clamp distance to max reach
      clampedDist = min dist maxReach

      -- Get joint limits based on effector type
      (minAngleY, maxAngleY) = getJointLimitsY effector

      -- Calculate current angle from horizontal (Y-axis rotation)
      -- Positive = up, Negative = down
      V3 dx dy dz = toTarget
      horizontalDist = sqrt (dx * dx + dz * dz)
      currentAngleY = if horizontalDist < 0.0001
                      then if dy >= 0 then 90 else (-90)
                      else atan2 dy horizontalDist * 180 / pi

      -- Distance-clamped target (no angle clamping)
      distanceClampedTarget = if dist <= maxReach
                              then targetPos
                              else rootPos ^+^ (normalize toTarget ^* maxReach)

      -- Angle-clamped target
      clampedAngleY = clamp minAngleY maxAngleY currentAngleY
      clampedAngleRad = clampedAngleY * pi / 180

      horizontalDir = if horizontalDist < 0.0001
                      then V3 1 0 0
                      else V3 (dx / horizontalDist) 0 (dz / horizontalDist)

      newHorizontalDist = clampedDist * cos clampedAngleRad
      newVerticalDist = clampedDist * sin clampedAngleRad
      V3 hx _ hz = horizontalDir

      angleClampedTarget = rootPos ^+^ V3 (hx * newHorizontalDist) newVerticalDist (hz * newHorizontalDist)

      -- Calculate blend factor based on how far outside limits
      -- Use soft margins for smooth transition (5 degrees)
      softMargin = 5.0

      -- How far outside the limits (0 = inside, positive = outside)
      overMin = max 0 (minAngleY - currentAngleY)  -- positive when below min
      overMax = max 0 (currentAngleY - maxAngleY)  -- positive when above max
      overLimit = max overMin overMax

      -- Blend factor: 0 = use original, 1 = use clamped
      -- Smoothly transition over the soft margin
      blendFactor = smoothstep 0 softMargin overLimit

  in lerpV3 distanceClampedTarget angleClampedTarget blendFactor
  where
    lerpV3 (V3 x1 y1 z1) (V3 x2 y2 z2) t =
      V3 (x1 + (x2 - x1) * t) (y1 + (y2 - y1) * t) (z1 + (z2 - z1) * t)

-- | Get Y-axis (Down-Up / Front-Back) joint limits in degrees
--
-- Returns (minAngle, maxAngle) where:
--   - Negative angles = down/back
--   - Positive angles = up/forward
getJointLimitsY :: HumanoidBone -> (Float, Float)
getJointLimitsY bone = case bone of
  -- Arms: Down-Up range is -60° to +100°
  RightHand     -> (-60, 100)
  RightLowerArm -> (-60, 100)
  LeftHand      -> (-60, 100)
  LeftLowerArm  -> (-60, 100)
  -- Legs: Front-Back range is -90° to +50°
  RightFoot     -> (-90, 50)
  RightToes     -> (-90, 50)
  RightLowerLeg -> (-90, 50)
  LeftFoot      -> (-90, 50)
  LeftToes      -> (-90, 50)
  LeftLowerLeg  -> (-90, 50)
  -- Default: wide range
  _ -> (-90, 90)
