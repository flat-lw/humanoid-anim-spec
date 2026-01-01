-- |
-- Module      : HumanoidAnim.IK.CCD
-- Description : Cyclic Coordinate Descent IK solver
--
-- This module implements the CCD (Cyclic Coordinate Descent) algorithm
-- for inverse kinematics. CCD works by iteratively rotating each joint
-- to minimize the distance between the end effector and the target.
module HumanoidAnim.IK.CCD
  ( -- * CCD Solver
    solveCCD
  , CCDConfig(..)
  , defaultCCDConfig
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Linear (V3(..), Quaternion(..), normalize, cross, dot, norm, (^-^), (^+^), (*^))
import qualified Linear.Quaternion as Q

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Hierarchy (boneParent, getBoneChain)
import HumanoidAnim.IK.Core

-- | CCD solver configuration
data CCDConfig = CCDConfig
  { ccdMaxIterations :: Int
    -- ^ Maximum number of iterations (default: 50)
  , ccdTolerance :: Float
    -- ^ Convergence tolerance in meters (default: 0.001)
  , ccdDampingFactor :: Float
    -- ^ Damping factor to prevent overshooting (default: 1.0)
  } deriving stock (Show, Eq)

-- | Default CCD configuration
defaultCCDConfig :: CCDConfig
defaultCCDConfig = CCDConfig
  { ccdMaxIterations = 50
  , ccdTolerance = 0.001
  , ccdDampingFactor = 1.0
  }

-- | Solve IK using CCD algorithm
solveCCD :: CCDConfig -> IKInput -> IKOutput
solveCCD config input =
  let skeleton = ikSkeleton input
      initialPose = ikInitialPose input
      constraints = ikConstraints input

      -- Extract fixed bones and effector
      fixedBones = [(bone, pos) | Fixed bone pos <- constraints]
      effectors = [(bone, pos) | Effector bone pos <- constraints]

      -- For now, handle single effector
      (effectorBone, targetPos) = case effectors of
        [(b, p)] -> (b, p)
        _ -> (RightHand, V3 0 1.4 0)  -- Default fallback

      -- Get the chain from fixed root to effector
      fixedRoot = case fixedBones of
        ((bone, _):_) -> bone
        [] -> Hips

      chain = getChainFromTo skeleton fixedRoot effectorBone

      -- Apply fixed positions
      poseWithFixed = foldl' (\p (bone, pos) -> Map.insert bone pos p) initialPose fixedBones

      -- Run CCD iterations
      (finalPose, iterations, converged, finalError) =
        runCCD config skeleton chain targetPos poseWithFixed

      -- Build warnings
      warnings = if not converged
                 then [DidNotConverge iterations finalError]
                 else []

  in IKOutput
       { ikResultPose = finalPose
       , ikIterations = iterations
       , ikConverged = converged
       , ikError = finalError
       , ikWarnings = warnings
       }

-- | Get chain of bones between two bones
getChainFromTo :: Skeleton -> HumanoidBone -> HumanoidBone -> [HumanoidBone]
getChainFromTo skeleton from to =
  -- Get chain from root to effector, then filter to active bones
  let effectorChain = getBoneChain to
      -- Find 'from' in the chain and take from there
      chainFromRoot = dropWhile (/= from) effectorChain
  in filter (`Set.member` skeletonActiveBones skeleton) chainFromRoot

-- | Run CCD iterations
runCCD :: CCDConfig
       -> Skeleton
       -> [HumanoidBone]
       -> V3 Float  -- Target position
       -> Map HumanoidBone (V3 Float)  -- Initial pose
       -> (Map HumanoidBone (V3 Float), Int, Bool, Float)
runCCD config skeleton chain target initialPose = go 0 initialPose
  where
    tolerance = ccdTolerance config
    maxIter = ccdMaxIterations config
    damping = ccdDampingFactor config

    go :: Int -> Map HumanoidBone (V3 Float) -> (Map HumanoidBone (V3 Float), Int, Bool, Float)
    go iter pose
      | iter >= maxIter = (pose, iter, False, currentError)
      | currentError < tolerance = (pose, iter, True, currentError)
      | otherwise = go (iter + 1) newPose
      where
        -- Get current end effector position
        effectorBone = last chain
        currentEffector = Map.findWithDefault (V3 0 0 0) effectorBone pose
        currentError = norm (target ^-^ currentEffector)

        -- Apply one full CCD iteration (iterate through all joints except effector)
        -- Go from root to effector (excluding effector itself)
        jointsToRotate = init chain

        newPose = foldl' (rotateJoint target damping) pose (reverse jointsToRotate)

-- | Rotate a single joint to minimize distance to target
rotateJoint :: V3 Float  -- Target
            -> Float     -- Damping factor
            -> Map HumanoidBone (V3 Float)  -- Current pose
            -> HumanoidBone  -- Joint to rotate
            -> Map HumanoidBone (V3 Float)
rotateJoint target damping pose joint =
  let jointPos = Map.findWithDefault (V3 0 0 0) joint pose

      -- Find end effector (last bone in the chain after this joint)
      -- For simplicity, we find all bones that could be affected
      allBones = Map.keys pose

      -- Get current effector position (rightmost/deepest bone in pose)
      effectorPos = case filter isArmBone allBones of
        [] -> Map.findWithDefault (V3 0 0 0) RightHand pose
        bones -> Map.findWithDefault (V3 0 0 0) (maximum bones) pose

      -- Vector from joint to current effector
      toEffector = effectorPos ^-^ jointPos
      toEffectorLen = norm toEffector

      -- Vector from joint to target
      toTarget = target ^-^ jointPos
      toTargetLen = norm toTarget

  in if toEffectorLen < 0.0001 || toTargetLen < 0.0001
     then pose  -- Skip if vectors are too small
     else
       let -- Normalize vectors
           toEffectorN = normalize toEffector
           toTargetN = normalize toTarget

           -- Calculate rotation axis (cross product)
           axis = cross toEffectorN toTargetN
           axisLen = norm axis

       in if axisLen < 0.0001
          then pose  -- Vectors are parallel, no rotation needed
          else
            let -- Calculate rotation angle
                cosAngle = clampF (-1) 1 (dot toEffectorN toTargetN)
                angle = acos cosAngle * damping

                -- Create rotation quaternion
                axisN = normalize axis
                rotation = axisAngleToQuat axisN angle

                -- Apply rotation to all bones after this joint
                -- (rotate their positions around the joint)
                newPose = Map.mapWithKey (rotateAround jointPos rotation joint) pose

            in newPose

-- | Check if a bone is an arm bone (for finding effector)
isArmBone :: HumanoidBone -> Bool
isArmBone bone = bone `elem`
  [LeftShoulder, LeftUpperArm, LeftLowerArm, LeftHand,
   RightShoulder, RightUpperArm, RightLowerArm, RightHand]

-- | Rotate a position around a pivot point
rotateAround :: V3 Float -> Quaternion Float -> HumanoidBone -> HumanoidBone -> V3 Float -> V3 Float
rotateAround pivot rotation pivotBone bone pos
  | boneOrder bone <= boneOrder pivotBone = pos  -- Don't rotate bones before pivot
  | otherwise =
      let offset = pos ^-^ pivot
          rotatedOffset = Q.rotate rotation offset
      in pivot ^+^ rotatedOffset
  where
    boneOrder = fromEnum

-- | Create quaternion from axis and angle
axisAngleToQuat :: V3 Float -> Float -> Quaternion Float
axisAngleToQuat axis angle =
  let halfAngle = angle / 2
      s = sin halfAngle
      c = cos halfAngle
  in Quaternion c (s *^ axis)

-- | Clamp a float value
clampF :: Float -> Float -> Float -> Float
clampF lo hi x = max lo (min hi x)
