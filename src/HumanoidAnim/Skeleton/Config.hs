-- |
-- Module      : HumanoidAnim.Skeleton.Config
-- Description : Skeleton configuration and construction
--
-- This module handles skeleton configuration including bone lengths,
-- rest poses, and skeleton construction.
module HumanoidAnim.Skeleton.Config
  ( -- * Configuration
    SkeletonConfig(..)
  , defaultSkeletonConfig

    -- * Skeleton Type
  , Skeleton(..)
  , buildSkeleton

    -- * Transform Type
  , Transform(..)

    -- * Default Values
  , defaultBoneLengths
  , defaultRestPositions
  , defaultRestPose
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V3 (..), Quaternion (..))

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Hierarchy

-- Forward declaration for Transform (will be imported properly later)
data Transform = Transform
  { transformPosition :: V3 Float
  , transformRotation :: Quaternion Float
  } deriving stock (Show, Eq)

-- | Skeleton configuration
data SkeletonConfig = SkeletonConfig
  { skeletonDetail :: SkeletonDetail
    -- ^ Level of detail (minimal, standard, full)
  , customBoneLengths :: Map HumanoidBone Float
    -- ^ Custom bone lengths (overrides defaults)
  , enabledBones :: Maybe (Set HumanoidBone)
    -- ^ Custom set of enabled bones (Nothing = use detail level)
  } deriving stock (Show, Eq)

-- | Default skeleton configuration (Standard detail)
defaultSkeletonConfig :: SkeletonConfig
defaultSkeletonConfig = SkeletonConfig
  { skeletonDetail = StandardSkeleton
  , customBoneLengths = Map.empty
  , enabledBones = Nothing
  }

-- | Built skeleton ready for use
data Skeleton = Skeleton
  { skeletonParents :: Map HumanoidBone (Maybe HumanoidBone)
    -- ^ Parent relationships
  , skeletonLengths :: Map HumanoidBone Float
    -- ^ Bone lengths in meters
  , skeletonRestPose :: Map HumanoidBone Transform
    -- ^ T-pose rest positions and rotations
  , skeletonActiveBones :: Set HumanoidBone
    -- ^ Currently active bones
  } deriving stock (Show, Eq)

-- | Build a skeleton from configuration
buildSkeleton :: SkeletonConfig -> Skeleton
buildSkeleton cfg = Skeleton
  { skeletonParents = Map.fromList [(b, boneParent b) | b <- activeBonesList]
  , skeletonLengths = Map.union (customBoneLengths cfg) $
                      Map.filterWithKey (\k _ -> k `Set.member` activeBones) defaultBoneLengths
  , skeletonRestPose = Map.filterWithKey (\k _ -> k `Set.member` activeBones) defaultRestPose
  , skeletonActiveBones = activeBones
  }
  where
    activeBones = case enabledBones cfg of
      Just bones -> bones
      Nothing -> Set.fromList $ bonesForDetail (skeletonDetail cfg)
    activeBonesList = Set.toList activeBones

-- | Default bone lengths in meters (based on ~1.7m height)
defaultBoneLengths :: Map HumanoidBone Float
defaultBoneLengths = Map.fromList
  -- Spine
  [ (Spine,         0.10)
  , (Chest,         0.15)
  , (UpperChest,    0.10)
  , (Neck,          0.10)
  , (Head,          0.15)

  -- Left arm
  , (LeftShoulder,  0.13)
  , (LeftUpperArm,  0.28)
  , (LeftLowerArm,  0.25)
  , (LeftHand,      0.10)

  -- Right arm
  , (RightShoulder, 0.13)
  , (RightUpperArm, 0.28)
  , (RightLowerArm, 0.25)
  , (RightHand,     0.10)

  -- Left leg
  , (LeftUpperLeg,  0.45)
  , (LeftLowerLeg,  0.45)
  , (LeftFoot,      0.15)
  , (LeftToes,      0.05)

  -- Right leg
  , (RightUpperLeg, 0.45)
  , (RightLowerLeg, 0.45)
  , (RightFoot,     0.15)
  , (RightToes,     0.05)

  -- Face
  , (Jaw,           0.05)
  , (LeftEye,       0.02)
  , (RightEye,      0.02)

  -- Fingers (approximate)
  , (LeftThumbProximal,      0.03)
  , (LeftThumbIntermediate,  0.02)
  , (LeftThumbDistal,        0.02)
  , (LeftIndexProximal,      0.03)
  , (LeftIndexIntermediate,  0.02)
  , (LeftIndexDistal,        0.02)
  , (LeftMiddleProximal,     0.035)
  , (LeftMiddleIntermediate, 0.025)
  , (LeftMiddleDistal,       0.02)
  , (LeftRingProximal,       0.03)
  , (LeftRingIntermediate,   0.02)
  , (LeftRingDistal,         0.02)
  , (LeftLittleProximal,     0.025)
  , (LeftLittleIntermediate, 0.015)
  , (LeftLittleDistal,       0.015)

  , (RightThumbProximal,      0.03)
  , (RightThumbIntermediate,  0.02)
  , (RightThumbDistal,        0.02)
  , (RightIndexProximal,      0.03)
  , (RightIndexIntermediate,  0.02)
  , (RightIndexDistal,        0.02)
  , (RightMiddleProximal,     0.035)
  , (RightMiddleIntermediate, 0.025)
  , (RightMiddleDistal,       0.02)
  , (RightRingProximal,       0.03)
  , (RightRingIntermediate,   0.02)
  , (RightRingDistal,         0.02)
  , (RightLittleProximal,     0.025)
  , (RightLittleIntermediate, 0.015)
  , (RightLittleDistal,       0.015)

  -- Twist bones
  , (UpperChestTwist, 0.05)
  , (SpineTwist,      0.05)
  ]

-- | Default rest positions (T-pose) in meters
defaultRestPositions :: Map HumanoidBone (V3 Float)
defaultRestPositions = Map.fromList
  -- Core
  [ (Hips,          V3   0.00  1.00  0.00)
  , (Spine,         V3   0.00  1.10  0.00)
  , (Chest,         V3   0.00  1.25  0.00)
  , (UpperChest,    V3   0.00  1.35  0.00)
  , (Neck,          V3   0.00  1.45  0.00)
  , (Head,          V3   0.00  1.55  0.00)

  -- Left arm (T-pose: arms horizontal)
  , (LeftShoulder,  V3   0.05  1.40  0.00)
  , (LeftUpperArm,  V3   0.18  1.40  0.00)
  , (LeftLowerArm,  V3   0.46  1.40  0.00)
  , (LeftHand,      V3   0.71  1.40  0.00)

  -- Right arm (T-pose: arms horizontal)
  , (RightShoulder, V3 (-0.05) 1.40  0.00)
  , (RightUpperArm, V3 (-0.18) 1.40  0.00)
  , (RightLowerArm, V3 (-0.46) 1.40  0.00)
  , (RightHand,     V3 (-0.71) 1.40  0.00)

  -- Left leg
  , (LeftUpperLeg,  V3   0.10  0.95  0.00)
  , (LeftLowerLeg,  V3   0.10  0.50  0.00)
  , (LeftFoot,      V3   0.10  0.05  0.00)
  , (LeftToes,      V3   0.10  0.00  0.10)

  -- Right leg
  , (RightUpperLeg, V3 (-0.10) 0.95  0.00)
  , (RightLowerLeg, V3 (-0.10) 0.50  0.00)
  , (RightFoot,     V3 (-0.10) 0.05  0.00)
  , (RightToes,     V3 (-0.10) 0.00  0.10)

  -- Face
  , (Jaw,           V3   0.00  1.50  0.05)
  , (LeftEye,       V3   0.03  1.60  0.08)
  , (RightEye,      V3 (-0.03) 1.60  0.08)
  ]

-- | Default rest pose with transforms
defaultRestPose :: Map HumanoidBone Transform
defaultRestPose = Map.map toTransform defaultRestPositions
  where
    identityQuat = Quaternion 1 (V3 0 0 0)
    toTransform pos = Transform pos identityQuat
