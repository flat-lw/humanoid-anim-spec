-- |
-- Module      : HumanoidAnim.Skeleton.Config
-- Description : Skeleton configuration and construction
--
-- This module handles skeleton configuration including bone lengths,
-- rest poses, and skeleton construction.
--
-- == Anthropometric Data Source
--
-- Bone lengths are based on 50th percentile adult female anthropometric data.
-- Source: Roy Mech Anthropometric Tables (https://roymech.org/Useful_Tables/Human/Human_sizes.html)
--
-- === Reference Body Measurements (50th percentile adult female)
--
-- @
-- Total Height:        1.610 m
-- Shoulder-Elbow:      0.330 m (upper arm)
-- Elbow-Wrist:         0.220 m (forearm, not including hand)
-- Hand Length:         0.175 m
-- Buttock-Knee:        0.585 m (thigh, includes sitting portion)
-- Popliteal Height:    0.410 m (lower leg)
-- Foot Length:         0.240 m
-- Biacromial Breadth:  0.365 m (shoulder to shoulder)
-- Sitting Height:      0.855 m (head + torso)
-- @
--
-- == Fetus Position
--
-- The default rest pose is "Fetus Position" which corresponds to Unity Humanoid
-- muscle value = 0 for all muscles. In this position:
--
-- * Arms hang down with elbows bent approximately 80-90 degrees
-- * Legs are raised forward with knees bent approximately 90 degrees
-- * Spine has a slight forward curve
--
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

    -- * Anthropometric Constants
  , baseHeight
  , shoulderWidth
  , hipWidth
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
    -- ^ Fetus Position rest pose (Unity Humanoid muscle=0 pose)
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

-- ============================================================================
-- Anthropometric Constants (50th percentile adult female)
-- ============================================================================

-- | Base height in meters (50th percentile adult female: 1.610m)
baseHeight :: Float
baseHeight = 1.610

-- | Shoulder width (biacromial breadth) in meters
-- 50th percentile: 0.365m, half = 0.1825m from center
shoulderWidth :: Float
shoulderWidth = 0.365

-- | Hip width in meters (estimated from pelvic breadth)
-- Approximately 0.36m for 50th percentile female
hipWidth :: Float
hipWidth = 0.36

-- ============================================================================
-- Bone Lengths
-- ============================================================================
--
-- All lengths are in meters, based on 50th percentile adult female data.
-- Source: Roy Mech Anthropometric Tables
--
-- Upper limb total: 0.330 (upper arm) + 0.220 (forearm) + 0.175 (hand) = 0.725m
-- Lower limb total: 0.400 (thigh) + 0.410 (lower leg) + 0.080 (ankle-floor) = 0.890m

-- | Default bone lengths in meters
-- Based on 50th percentile adult female anthropometric data
defaultBoneLengths :: Map HumanoidBone Float
defaultBoneLengths = Map.fromList
  -- === Spine ===
  -- Sitting height (0.855m) minus head (~0.20m) = ~0.655m for spine+neck
  -- Distributed across: Hips→Spine, Spine→Chest, Chest→UpperChest, UpperChest→Neck
  [ (Spine,         0.12)   -- Hips to Spine
  , (Chest,         0.14)   -- Spine to Chest
  , (UpperChest,    0.12)   -- Chest to UpperChest
  , (Neck,          0.10)   -- UpperChest to Neck
  , (Head,          0.20)   -- Neck to top of Head

  -- === Left Arm ===
  -- Shoulder-Elbow: 0.330m, Elbow-Wrist: 0.220m, Hand: 0.175m
  , (LeftShoulder,  0.12)   -- Center to shoulder joint (half of biacromial - some)
  , (LeftUpperArm,  0.33)   -- Shoulder to Elbow (measured data: 0.330m)
  , (LeftLowerArm,  0.22)   -- Elbow to Wrist (measured data: 0.220m)
  , (LeftHand,      0.175)  -- Wrist to fingertip (measured data: 0.175m)

  -- === Right Arm ===
  , (RightShoulder, 0.12)
  , (RightUpperArm, 0.33)
  , (RightLowerArm, 0.22)
  , (RightHand,     0.175)

  -- === Left Leg ===
  -- Thigh (hip to knee): ~0.40m, Lower leg: 0.410m, Foot height: ~0.08m
  , (LeftUpperLeg,  0.40)   -- Hip to Knee
  , (LeftLowerLeg,  0.41)   -- Knee to Ankle (popliteal height: 0.410m)
  , (LeftFoot,      0.08)   -- Ankle to floor
  , (LeftToes,      0.10)   -- Heel to toe tip

  -- === Right Leg ===
  , (RightUpperLeg, 0.40)
  , (RightLowerLeg, 0.41)
  , (RightFoot,     0.08)
  , (RightToes,     0.10)

  -- === Face ===
  , (Jaw,           0.05)
  , (LeftEye,       0.02)
  , (RightEye,      0.02)

  -- === Fingers (approximate, based on hand length 0.175m) ===
  -- Fingers are roughly 45% of hand length for middle finger
  , (LeftThumbProximal,      0.035)
  , (LeftThumbIntermediate,  0.025)
  , (LeftThumbDistal,        0.020)
  , (LeftIndexProximal,      0.040)
  , (LeftIndexIntermediate,  0.025)
  , (LeftIndexDistal,        0.020)
  , (LeftMiddleProximal,     0.045)
  , (LeftMiddleIntermediate, 0.028)
  , (LeftMiddleDistal,       0.022)
  , (LeftRingProximal,       0.040)
  , (LeftRingIntermediate,   0.025)
  , (LeftRingDistal,         0.020)
  , (LeftLittleProximal,     0.032)
  , (LeftLittleIntermediate, 0.020)
  , (LeftLittleDistal,       0.018)

  , (RightThumbProximal,      0.035)
  , (RightThumbIntermediate,  0.025)
  , (RightThumbDistal,        0.020)
  , (RightIndexProximal,      0.040)
  , (RightIndexIntermediate,  0.025)
  , (RightIndexDistal,        0.020)
  , (RightMiddleProximal,     0.045)
  , (RightMiddleIntermediate, 0.028)
  , (RightMiddleDistal,       0.022)
  , (RightRingProximal,       0.040)
  , (RightRingIntermediate,   0.025)
  , (RightRingDistal,         0.020)
  , (RightLittleProximal,     0.032)
  , (RightLittleIntermediate, 0.020)
  , (RightLittleDistal,       0.018)

  -- === Twist bones ===
  , (UpperChestTwist, 0.06)
  , (SpineTwist,      0.06)
  ]

-- ============================================================================
-- Fetus Position (Rest Pose)
-- ============================================================================
--
-- Unity Humanoid muscle=0 corresponds to "Fetus Position":
-- - Arms hang down with elbows bent ~90 degrees, forearms pointing forward-up
-- - Legs raised forward with knees bent ~90 degrees
-- - Spine has slight forward curve
--
-- Positions are calculated from bone lengths to ensure consistency.
-- Hips are placed at Y=0.89m (leg length when straight: 0.40+0.41+0.08 = 0.89m)

-- | Default rest positions (Fetus Position - Unity Humanoid muscle=0 pose) in meters
defaultRestPositions :: Map HumanoidBone (V3 Float)
defaultRestPositions = Map.fromList
  -- === Core/Spine ===
  -- Hips at standing height (leg length from floor)
  -- Total leg: 0.40 (thigh) + 0.41 (shin) + 0.08 (foot) = 0.89m
  [ (Hips,          V3   0.00  0.89  0.00)
  -- Spine bones stack up with slight forward curve
  , (Spine,         V3   0.00  1.01  0.02)   -- +0.12m up, slight forward
  , (Chest,         V3   0.00  1.15  0.03)   -- +0.14m up
  , (UpperChest,    V3   0.00  1.27  0.03)   -- +0.12m up
  , (Neck,          V3   0.00  1.37  0.02)   -- +0.10m up
  , (Head,          V3   0.00  1.57  0.00)   -- +0.20m up

  -- === Left Arm (Fetus Position) ===
  -- Shoulder at UpperChest height, offset by half shoulder width
  -- Upper arm hangs down (Y decreases by ~0.30m)
  -- Elbow bent ~90 degrees, forearm goes forward and slightly up
  -- Unity coordinate: X+ = Right, so Left side uses X-
  , (LeftShoulder,  V3 (-0.06) 1.27  0.02)   -- Just off center (left side = X-)
  , (LeftUpperArm,  V3 (-0.18) 1.27  0.02)   -- Shoulder joint (-0.12m out)
  , (LeftLowerArm,  V3 (-0.20) 0.94  0.05)   -- Elbow: arm hangs down (-0.33m)
  , (LeftHand,      V3 (-0.20) 0.96  0.27)   -- Wrist: forearm forward (+0.22m Z)

  -- === Right Arm (Fetus Position) ===
  -- Unity coordinate: X+ = Right, so Right side uses X+
  , (RightShoulder, V3   0.06  1.27  0.02)
  , (RightUpperArm, V3   0.18  1.27  0.02)
  , (RightLowerArm, V3   0.20  0.94  0.05)
  , (RightHand,     V3   0.20  0.96  0.27)

  -- === Left Leg (Fetus Position) ===
  -- Hip joint offset by half hip width
  -- Thigh goes forward (Z+) and down (Y-), ~45 degree angle
  -- Knee bent ~90 degrees, shin goes down
  -- Unity coordinate: X+ = Right, so Left side uses X-
  , (LeftUpperLeg,  V3 (-0.09) 0.89  0.00)   -- Hip joint (left side = X-)
  , (LeftLowerLeg,  V3 (-0.10) 0.61  0.28)   -- Knee: thigh ~45deg forward (-0.28 Y, +0.28 Z)
  , (LeftFoot,      V3 (-0.10) 0.20  0.26)   -- Ankle: shin straight down (-0.41m Y)
  , (LeftToes,      V3 (-0.10) 0.12  0.34)   -- Toes forward

  -- === Right Leg (Fetus Position) ===
  -- Unity coordinate: X+ = Right, so Right side uses X+
  , (RightUpperLeg, V3   0.09  0.89  0.00)
  , (RightLowerLeg, V3   0.10  0.61  0.28)
  , (RightFoot,     V3   0.10  0.20  0.26)
  , (RightToes,     V3   0.10  0.12  0.34)

  -- === Face ===
  , (Jaw,           V3   0.00  1.50  0.08)
  , (LeftEye,       V3 (-0.03) 1.55  0.10)   -- Left side = X-
  , (RightEye,      V3   0.03  1.55  0.10)   -- Right side = X+
  ]

-- | Default rest pose with transforms
defaultRestPose :: Map HumanoidBone Transform
defaultRestPose = Map.map toTransform defaultRestPositions
  where
    identityQuat = Quaternion 1 (V3 0 0 0)
    toTransform pos = Transform pos identityQuat
