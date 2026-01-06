-- |
-- Module      : HumanoidAnim.Output.Muscle
-- Description : Unity Humanoid Muscle system definitions and conversion
--
-- This module provides the muscle definitions and conversion functions
-- for Unity's Humanoid animation system. Muscles are normalized values
-- between -1.0 and 1.0 that control joint rotations.
module HumanoidAnim.Output.Muscle
  ( -- * Muscle Types
    MuscleId(..)
  , MuscleRange(..)
  , MuscleValue

    -- * Muscle Definitions
  , allMuscles
  , musclePropertyName
  , muscleDefaultRange

    -- * Bone Muscle Configuration
  , BoneMuscleConfig(..)
  , MuscleAxisConfig(..)
  , RotationAxis(..)
  , boneMuscleDatabase
  , findBoneConfig

    -- * Conversion
  , quaternionToMuscles
  , musclesToQuaternion
  , boneToMuscles
  , positionToMuscles
  , fetusDirection
  , fetusRotation
  , fetusPositions

    -- * Forward Kinematics
  , musclesToPose
  , musclesToTransforms
  , sampleMusclesToPose
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Skeleton.Bones (HumanoidBone(..), allBones)
import HumanoidAnim.Skeleton.Config (defaultSkeletonConfig, buildSkeleton, Transform(..), Skeleton(..))
import HumanoidAnim.Skeleton.Hierarchy (boneParent, getDepth)
import HumanoidAnim.Skeleton.Transform (computeRotations, fetusPoseMuscleAxes, quaternionToAxisAngles, quaternionFromAxisAngle, MuscleAxes(..))

-- | Muscle value (normalized to -1.0 to 1.0)
type MuscleValue = Float

-- | Muscle range in degrees
data MuscleRange = MuscleRange
  { muscleMin :: Float  -- ^ Minimum angle in degrees
  , muscleMax :: Float  -- ^ Maximum angle in degrees
  } deriving stock (Show, Eq)

-- | Unity Humanoid Muscle identifiers
-- These correspond to Unity's muscle property names
data MuscleId
  -- Spine
  = SpineFrontBack
  | SpineLeftRight
  | SpineTwistLeftRight
  | ChestFrontBack
  | ChestLeftRight
  | ChestTwistLeftRight
  | UpperChestFrontBack
  | UpperChestLeftRight
  | UpperChestTwistLeftRight

  -- Neck & Head
  | NeckNodDownUp
  | NeckTiltLeftRight
  | NeckTurnLeftRight
  | HeadNodDownUp
  | HeadTiltLeftRight
  | HeadTurnLeftRight
  | JawClose
  | JawLeftRight

  -- Eyes
  | LeftEyeDownUp
  | LeftEyeInOut
  | RightEyeDownUp
  | RightEyeInOut

  -- Left Arm
  | LeftShoulderDownUp
  | LeftShoulderFrontBack
  | LeftArmDownUp
  | LeftArmFrontBack
  | LeftArmTwistInOut
  | LeftForearmStretch
  | LeftForearmTwistInOut
  | LeftHandDownUp
  | LeftHandInOut

  -- Right Arm
  | RightShoulderDownUp
  | RightShoulderFrontBack
  | RightArmDownUp
  | RightArmFrontBack
  | RightArmTwistInOut
  | RightForearmStretch
  | RightForearmTwistInOut
  | RightHandDownUp
  | RightHandInOut

  -- Left Leg
  | LeftUpperLegFrontBack
  | LeftUpperLegInOut
  | LeftUpperLegTwistInOut
  | LeftLegStretch
  | LeftLegTwistInOut
  | LeftFootUpDown
  | LeftFootTwistInOut
  | LeftToesUpDown

  -- Right Leg
  | RightUpperLegFrontBack
  | RightUpperLegInOut
  | RightUpperLegTwistInOut
  | RightLegStretch
  | RightLegTwistInOut
  | RightFootUpDown
  | RightFootTwistInOut
  | RightToesUpDown

  -- Left Hand Fingers
  | LeftThumb1Stretched
  | LeftThumbSpread
  | LeftThumb2Stretched
  | LeftThumb3Stretched
  | LeftIndex1Stretched
  | LeftIndexSpread
  | LeftIndex2Stretched
  | LeftIndex3Stretched
  | LeftMiddle1Stretched
  | LeftMiddleSpread
  | LeftMiddle2Stretched
  | LeftMiddle3Stretched
  | LeftRing1Stretched
  | LeftRingSpread
  | LeftRing2Stretched
  | LeftRing3Stretched
  | LeftLittle1Stretched
  | LeftLittleSpread
  | LeftLittle2Stretched
  | LeftLittle3Stretched

  -- Right Hand Fingers
  | RightThumb1Stretched
  | RightThumbSpread
  | RightThumb2Stretched
  | RightThumb3Stretched
  | RightIndex1Stretched
  | RightIndexSpread
  | RightIndex2Stretched
  | RightIndex3Stretched
  | RightMiddle1Stretched
  | RightMiddleSpread
  | RightMiddle2Stretched
  | RightMiddle3Stretched
  | RightRing1Stretched
  | RightRingSpread
  | RightRing2Stretched
  | RightRing3Stretched
  | RightLittle1Stretched
  | RightLittleSpread
  | RightLittle2Stretched
  | RightLittle3Stretched
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | All muscle IDs
allMuscles :: [MuscleId]
allMuscles = [minBound .. maxBound]

-- | Get Unity property name for a muscle
musclePropertyName :: MuscleId -> String
musclePropertyName mid = case mid of
  -- Spine
  SpineFrontBack -> "Spine Front-Back"
  SpineLeftRight -> "Spine Left-Right"
  SpineTwistLeftRight -> "Spine Twist Left-Right"
  ChestFrontBack -> "Chest Front-Back"
  ChestLeftRight -> "Chest Left-Right"
  ChestTwistLeftRight -> "Chest Twist Left-Right"
  UpperChestFrontBack -> "UpperChest Front-Back"
  UpperChestLeftRight -> "UpperChest Left-Right"
  UpperChestTwistLeftRight -> "UpperChest Twist Left-Right"

  -- Neck & Head
  NeckNodDownUp -> "Neck Nod Down-Up"
  NeckTiltLeftRight -> "Neck Tilt Left-Right"
  NeckTurnLeftRight -> "Neck Turn Left-Right"
  HeadNodDownUp -> "Head Nod Down-Up"
  HeadTiltLeftRight -> "Head Tilt Left-Right"
  HeadTurnLeftRight -> "Head Turn Left-Right"
  JawClose -> "Jaw Close"
  JawLeftRight -> "Jaw Left-Right"

  -- Eyes
  LeftEyeDownUp -> "Left Eye Down-Up"
  LeftEyeInOut -> "Left Eye In-Out"
  RightEyeDownUp -> "Right Eye Down-Up"
  RightEyeInOut -> "Right Eye In-Out"

  -- Left Arm
  LeftShoulderDownUp -> "Left Shoulder Down-Up"
  LeftShoulderFrontBack -> "Left Shoulder Front-Back"
  LeftArmDownUp -> "Left Arm Down-Up"
  LeftArmFrontBack -> "Left Arm Front-Back"
  LeftArmTwistInOut -> "Left Arm Twist In-Out"
  LeftForearmStretch -> "Left Forearm Stretch"
  LeftForearmTwistInOut -> "Left Forearm Twist In-Out"
  LeftHandDownUp -> "Left Hand Down-Up"
  LeftHandInOut -> "Left Hand In-Out"

  -- Right Arm
  RightShoulderDownUp -> "Right Shoulder Down-Up"
  RightShoulderFrontBack -> "Right Shoulder Front-Back"
  RightArmDownUp -> "Right Arm Down-Up"
  RightArmFrontBack -> "Right Arm Front-Back"
  RightArmTwistInOut -> "Right Arm Twist In-Out"
  RightForearmStretch -> "Right Forearm Stretch"
  RightForearmTwistInOut -> "Right Forearm Twist In-Out"
  RightHandDownUp -> "Right Hand Down-Up"
  RightHandInOut -> "Right Hand In-Out"

  -- Left Leg
  LeftUpperLegFrontBack -> "Left Upper Leg Front-Back"
  LeftUpperLegInOut -> "Left Upper Leg In-Out"
  LeftUpperLegTwistInOut -> "Left Upper Leg Twist In-Out"
  LeftLegStretch -> "Left Lower Leg Stretch"
  LeftLegTwistInOut -> "Left Lower Leg Twist In-Out"
  LeftFootUpDown -> "Left Foot Up-Down"
  LeftFootTwistInOut -> "Left Foot Twist In-Out"
  LeftToesUpDown -> "Left Toes Up-Down"

  -- Right Leg
  RightUpperLegFrontBack -> "Right Upper Leg Front-Back"
  RightUpperLegInOut -> "Right Upper Leg In-Out"
  RightUpperLegTwistInOut -> "Right Upper Leg Twist In-Out"
  RightLegStretch -> "Right Lower Leg Stretch"
  RightLegTwistInOut -> "Right Lower Leg Twist In-Out"
  RightFootUpDown -> "Right Foot Up-Down"
  RightFootTwistInOut -> "Right Foot Twist In-Out"
  RightToesUpDown -> "Right Toes Up-Down"

  -- Left Hand Fingers
  LeftThumb1Stretched -> "LeftHand.Thumb.1 Stretched"
  LeftThumbSpread -> "LeftHand.Thumb Spread"
  LeftThumb2Stretched -> "LeftHand.Thumb.2 Stretched"
  LeftThumb3Stretched -> "LeftHand.Thumb.3 Stretched"
  LeftIndex1Stretched -> "LeftHand.Index.1 Stretched"
  LeftIndexSpread -> "LeftHand.Index Spread"
  LeftIndex2Stretched -> "LeftHand.Index.2 Stretched"
  LeftIndex3Stretched -> "LeftHand.Index.3 Stretched"
  LeftMiddle1Stretched -> "LeftHand.Middle.1 Stretched"
  LeftMiddleSpread -> "LeftHand.Middle Spread"
  LeftMiddle2Stretched -> "LeftHand.Middle.2 Stretched"
  LeftMiddle3Stretched -> "LeftHand.Middle.3 Stretched"
  LeftRing1Stretched -> "LeftHand.Ring.1 Stretched"
  LeftRingSpread -> "LeftHand.Ring Spread"
  LeftRing2Stretched -> "LeftHand.Ring.2 Stretched"
  LeftRing3Stretched -> "LeftHand.Ring.3 Stretched"
  LeftLittle1Stretched -> "LeftHand.Little.1 Stretched"
  LeftLittleSpread -> "LeftHand.Little Spread"
  LeftLittle2Stretched -> "LeftHand.Little.2 Stretched"
  LeftLittle3Stretched -> "LeftHand.Little.3 Stretched"

  -- Right Hand Fingers
  RightThumb1Stretched -> "RightHand.Thumb.1 Stretched"
  RightThumbSpread -> "RightHand.Thumb Spread"
  RightThumb2Stretched -> "RightHand.Thumb.2 Stretched"
  RightThumb3Stretched -> "RightHand.Thumb.3 Stretched"
  RightIndex1Stretched -> "RightHand.Index.1 Stretched"
  RightIndexSpread -> "RightHand.Index Spread"
  RightIndex2Stretched -> "RightHand.Index.2 Stretched"
  RightIndex3Stretched -> "RightHand.Index.3 Stretched"
  RightMiddle1Stretched -> "RightHand.Middle.1 Stretched"
  RightMiddleSpread -> "RightHand.Middle Spread"
  RightMiddle2Stretched -> "RightHand.Middle.2 Stretched"
  RightMiddle3Stretched -> "RightHand.Middle.3 Stretched"
  RightRing1Stretched -> "RightHand.Ring.1 Stretched"
  RightRingSpread -> "RightHand.Ring Spread"
  RightRing2Stretched -> "RightHand.Ring.2 Stretched"
  RightRing3Stretched -> "RightHand.Ring.3 Stretched"
  RightLittle1Stretched -> "RightHand.Little.1 Stretched"
  RightLittleSpread -> "RightHand.Little Spread"
  RightLittle2Stretched -> "RightHand.Little.2 Stretched"
  RightLittle3Stretched -> "RightHand.Little.3 Stretched"

-- | Default muscle range for each muscle type
muscleDefaultRange :: MuscleId -> MuscleRange
muscleDefaultRange mid = case mid of
  -- Spine (moderate range)
  SpineFrontBack -> MuscleRange (-40) 40
  SpineLeftRight -> MuscleRange (-40) 40
  SpineTwistLeftRight -> MuscleRange (-40) 40
  ChestFrontBack -> MuscleRange (-40) 40
  ChestLeftRight -> MuscleRange (-40) 40
  ChestTwistLeftRight -> MuscleRange (-40) 40
  UpperChestFrontBack -> MuscleRange (-20) 20
  UpperChestLeftRight -> MuscleRange (-20) 20
  UpperChestTwistLeftRight -> MuscleRange (-20) 20

  -- Neck & Head
  NeckNodDownUp -> MuscleRange (-40) 40
  NeckTiltLeftRight -> MuscleRange (-40) 40
  NeckTurnLeftRight -> MuscleRange (-40) 40
  HeadNodDownUp -> MuscleRange (-40) 40
  HeadTiltLeftRight -> MuscleRange (-40) 40
  HeadTurnLeftRight -> MuscleRange (-40) 40
  JawClose -> MuscleRange (-10) 10
  JawLeftRight -> MuscleRange (-10) 10

  -- Eyes
  LeftEyeDownUp -> MuscleRange (-15) 15
  LeftEyeInOut -> MuscleRange (-20) 20
  RightEyeDownUp -> MuscleRange (-15) 15
  RightEyeInOut -> MuscleRange (-20) 20

  -- Shoulder
  LeftShoulderDownUp -> MuscleRange (-15) 30
  LeftShoulderFrontBack -> MuscleRange (-15) 15
  RightShoulderDownUp -> MuscleRange (-15) 30
  RightShoulderFrontBack -> MuscleRange (-15) 15

  -- Upper Arm
  LeftArmDownUp -> MuscleRange (-60) 100
  LeftArmFrontBack -> MuscleRange (-100) 60
  LeftArmTwistInOut -> MuscleRange (-90) 90
  RightArmDownUp -> MuscleRange (-60) 100
  RightArmFrontBack -> MuscleRange (-100) 60
  RightArmTwistInOut -> MuscleRange (-90) 90

  -- Forearm
  LeftForearmStretch -> MuscleRange 0 150
  LeftForearmTwistInOut -> MuscleRange (-90) 90
  RightForearmStretch -> MuscleRange 0 150
  RightForearmTwistInOut -> MuscleRange (-90) 90

  -- Hand
  LeftHandDownUp -> MuscleRange (-80) 80
  LeftHandInOut -> MuscleRange (-40) 40
  RightHandDownUp -> MuscleRange (-80) 80
  RightHandInOut -> MuscleRange (-40) 40

  -- Upper Leg
  LeftUpperLegFrontBack -> MuscleRange (-90) 50
  LeftUpperLegInOut -> MuscleRange (-60) 60
  LeftUpperLegTwistInOut -> MuscleRange (-60) 60
  RightUpperLegFrontBack -> MuscleRange (-90) 50
  RightUpperLegInOut -> MuscleRange (-60) 60
  RightUpperLegTwistInOut -> MuscleRange (-60) 60

  -- Lower Leg
  LeftLegStretch -> MuscleRange 0 145
  LeftLegTwistInOut -> MuscleRange (-60) 60
  RightLegStretch -> MuscleRange 0 145
  RightLegTwistInOut -> MuscleRange (-60) 60

  -- Foot
  LeftFootUpDown -> MuscleRange (-50) 50
  LeftFootTwistInOut -> MuscleRange (-30) 30
  LeftToesUpDown -> MuscleRange (-50) 50
  RightFootUpDown -> MuscleRange (-50) 50
  RightFootTwistInOut -> MuscleRange (-30) 30
  RightToesUpDown -> MuscleRange (-50) 50

  -- Fingers (all similar range)
  _ -> MuscleRange (-20) 20  -- Default for fingers

-- | Get the center angle (Muscle=0 angle) for a muscle
--
-- From unity_humanoid_center_of_mass.md:
--   - Most muscles have center=0 (Muscle=0 at T-pose angle 0)
--   - Some muscles have non-zero center due to asymmetric range
--
-- Key values from documentation:
--   - Upper Leg Front-Back: center = -30 (T-Pose Muscle = 0.6)
--   - Lower Leg Stretch: center = -80 (T-Pose = fully extended)
--   - Arm Down-Up: center = -40 (T-Pose Muscle = 0.4)
--   - Arm Front-Back: center = -30 (T-Pose Muscle = 0.3)
--   - Forearm Stretch: center = -80 (T-Pose = fully extended)
muscleCenter :: MuscleId -> Float
muscleCenter mid = case mid of
  -- Upper Leg Front-Back: center = -30
  LeftUpperLegFrontBack  -> (-30)
  RightUpperLegFrontBack -> (-30)

  -- Lower Leg Stretch: center = -80
  LeftLegStretch  -> (-80)
  RightLegStretch -> (-80)

  -- Arm Down-Up: center = -40
  LeftArmDownUp  -> (-40)
  RightArmDownUp -> (-40)

  -- Arm Front-Back: center = -30
  LeftArmFrontBack  -> (-30)
  RightArmFrontBack -> (-30)

  -- Forearm Stretch: center = -80
  LeftForearmStretch  -> (-80)
  RightForearmStretch -> (-80)

  -- All other muscles: center = 0
  _ -> 0

-- | Get the muscles associated with a bone
boneToMuscles :: HumanoidBone -> [MuscleId]
boneToMuscles bone = case bone of
  Spine -> [SpineFrontBack, SpineLeftRight, SpineTwistLeftRight]
  Chest -> [ChestFrontBack, ChestLeftRight, ChestTwistLeftRight]
  UpperChest -> [UpperChestFrontBack, UpperChestLeftRight, UpperChestTwistLeftRight]
  Neck -> [NeckNodDownUp, NeckTiltLeftRight, NeckTurnLeftRight]
  Head -> [HeadNodDownUp, HeadTiltLeftRight, HeadTurnLeftRight]
  Jaw -> [JawClose, JawLeftRight]

  LeftEye -> [LeftEyeDownUp, LeftEyeInOut]
  RightEye -> [RightEyeDownUp, RightEyeInOut]

  LeftShoulder -> [LeftShoulderDownUp, LeftShoulderFrontBack]
  LeftUpperArm -> [LeftArmDownUp, LeftArmFrontBack, LeftArmTwistInOut]
  LeftLowerArm -> [LeftForearmStretch, LeftForearmTwistInOut]
  LeftHand -> [LeftHandDownUp, LeftHandInOut]

  RightShoulder -> [RightShoulderDownUp, RightShoulderFrontBack]
  RightUpperArm -> [RightArmDownUp, RightArmFrontBack, RightArmTwistInOut]
  RightLowerArm -> [RightForearmStretch, RightForearmTwistInOut]
  RightHand -> [RightHandDownUp, RightHandInOut]

  LeftUpperLeg -> [LeftUpperLegFrontBack, LeftUpperLegInOut, LeftUpperLegTwistInOut]
  LeftLowerLeg -> [LeftLegStretch, LeftLegTwistInOut]
  LeftFoot -> [LeftFootUpDown, LeftFootTwistInOut]
  LeftToes -> [LeftToesUpDown]

  RightUpperLeg -> [RightUpperLegFrontBack, RightUpperLegInOut, RightUpperLegTwistInOut]
  RightLowerLeg -> [RightLegStretch, RightLegTwistInOut]
  RightFoot -> [RightFootUpDown, RightFootTwistInOut]
  RightToes -> [RightToesUpDown]

  -- Fingers - Left
  LeftThumbProximal -> [LeftThumb1Stretched, LeftThumbSpread]
  LeftThumbIntermediate -> [LeftThumb2Stretched]
  LeftThumbDistal -> [LeftThumb3Stretched]
  LeftIndexProximal -> [LeftIndex1Stretched, LeftIndexSpread]
  LeftIndexIntermediate -> [LeftIndex2Stretched]
  LeftIndexDistal -> [LeftIndex3Stretched]
  LeftMiddleProximal -> [LeftMiddle1Stretched, LeftMiddleSpread]
  LeftMiddleIntermediate -> [LeftMiddle2Stretched]
  LeftMiddleDistal -> [LeftMiddle3Stretched]
  LeftRingProximal -> [LeftRing1Stretched, LeftRingSpread]
  LeftRingIntermediate -> [LeftRing2Stretched]
  LeftRingDistal -> [LeftRing3Stretched]
  LeftLittleProximal -> [LeftLittle1Stretched, LeftLittleSpread]
  LeftLittleIntermediate -> [LeftLittle2Stretched]
  LeftLittleDistal -> [LeftLittle3Stretched]

  -- Fingers - Right
  RightThumbProximal -> [RightThumb1Stretched, RightThumbSpread]
  RightThumbIntermediate -> [RightThumb2Stretched]
  RightThumbDistal -> [RightThumb3Stretched]
  RightIndexProximal -> [RightIndex1Stretched, RightIndexSpread]
  RightIndexIntermediate -> [RightIndex2Stretched]
  RightIndexDistal -> [RightIndex3Stretched]
  RightMiddleProximal -> [RightMiddle1Stretched, RightMiddleSpread]
  RightMiddleIntermediate -> [RightMiddle2Stretched]
  RightMiddleDistal -> [RightMiddle3Stretched]
  RightRingProximal -> [RightRing1Stretched, RightRingSpread]
  RightRingIntermediate -> [RightRing2Stretched]
  RightRingDistal -> [RightRing3Stretched]
  RightLittleProximal -> [RightLittle1Stretched, RightLittleSpread]
  RightLittleIntermediate -> [RightLittle2Stretched]
  RightLittleDistal -> [RightLittle3Stretched]

  -- Hips and other bones without muscles
  _ -> []

-- ============================================================================
-- Bone Muscle Configuration Database
-- ============================================================================

-- | Rotation axis for angle calculation
-- Matches Unity Humanoid specification where each muscle is defined by rotation around a specific axis
data RotationAxis
  = RotateAroundX Bool    -- ^ X-axis rotation (Twist). Bool = signFlip for left/right symmetry
  | RotateAroundY Bool    -- ^ Y-axis rotation (Front-Back for arms, In-Out for legs)
  | RotateAroundZ Bool    -- ^ Z-axis rotation (Down-Up for arms, Front-Back for legs)
  | NoRotation            -- ^ Twist axis - cannot be determined from position alone
  deriving stock (Show, Eq)

-- | Configuration for a single muscle axis
data MuscleAxisConfig = MuscleAxisConfig
  { macMuscleId :: MuscleId           -- ^ The muscle ID for this axis
  , macRotationAxis :: RotationAxis   -- ^ Which axis to measure rotation around
  } deriving stock (Show, Eq)

-- | Complete muscle configuration for a bone
data BoneMuscleConfig = BoneMuscleConfig
  { bmcBone :: HumanoidBone           -- ^ The bone this config applies to
  , bmcAxes :: [MuscleAxisConfig]     -- ^ Configuration for each muscle axis
  , bmcIsRight :: Bool                -- ^ Whether this is a right-side bone
  } deriving stock (Show, Eq)

-- | Database of bone muscle configurations
-- This centralizes all the bone-specific logic for position-based muscle calculation
--
-- Unity Humanoid axis conventions (from unity_humanoid_muscle_reference.md):
--   Upper Leg: X=Twist, Y=In-Out, Z=Front-Back
--   Lower Leg: X=Twist, Z=Stretch
--   Upper Arm: X=Twist, Y=Front-Back, Z=Down-Up
--   Lower Arm: X=Twist, Z=Stretch
boneMuscleDatabase :: [BoneMuscleConfig]
boneMuscleDatabase =
  -- Upper Legs
  -- Front-Back: Z-axis rotation
  -- In-Out: Y-axis rotation
  -- Right leg: outward = +X, so no flip needed
  -- Left leg: outward = -X, so flip needed to make outward = positive
  [ BoneMuscleConfig
      { bmcBone = RightUpperLeg
      , bmcAxes =
          [ MuscleAxisConfig RightUpperLegFrontBack (RotateAroundZ False)
          , MuscleAxisConfig RightUpperLegInOut (RotateAroundY False)
          , MuscleAxisConfig RightUpperLegTwistInOut NoRotation
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftUpperLeg
      , bmcAxes =
          [ MuscleAxisConfig LeftUpperLegFrontBack (RotateAroundZ True)   -- Left: flip for symmetry
          , MuscleAxisConfig LeftUpperLegInOut (RotateAroundY True)       -- Left: flip for symmetry
          , MuscleAxisConfig LeftUpperLegTwistInOut NoRotation
          ]
      , bmcIsRight = False
      }

  -- Upper Arms
  -- Down-Up: Z-axis rotation
  -- Front-Back: Y-axis rotation
  , BoneMuscleConfig
      { bmcBone = RightUpperArm
      , bmcAxes =
          [ MuscleAxisConfig RightArmDownUp (RotateAroundZ False)
          , MuscleAxisConfig RightArmFrontBack (RotateAroundY False)
          , MuscleAxisConfig RightArmTwistInOut NoRotation
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftUpperArm
      , bmcAxes =
          [ MuscleAxisConfig LeftArmDownUp (RotateAroundZ True)   -- Left: flip for symmetry
          , MuscleAxisConfig LeftArmFrontBack (RotateAroundY False)
          , MuscleAxisConfig LeftArmTwistInOut NoRotation
          ]
      , bmcIsRight = False
      }

  -- Lower Legs
  -- Stretch: Z-axis rotation
  , BoneMuscleConfig
      { bmcBone = RightLowerLeg
      , bmcAxes =
          [ MuscleAxisConfig RightLegStretch (RotateAroundZ False)
          , MuscleAxisConfig RightLegTwistInOut NoRotation
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftLowerLeg
      , bmcAxes =
          [ MuscleAxisConfig LeftLegStretch (RotateAroundZ False)
          , MuscleAxisConfig LeftLegTwistInOut NoRotation
          ]
      , bmcIsRight = False
      }

  -- Lower Arms (Forearms)
  -- Stretch: Z-axis rotation
  , BoneMuscleConfig
      { bmcBone = RightLowerArm
      , bmcAxes =
          [ MuscleAxisConfig RightForearmStretch (RotateAroundZ False)
          , MuscleAxisConfig RightForearmTwistInOut NoRotation
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftLowerArm
      , bmcAxes =
          [ MuscleAxisConfig LeftForearmStretch (RotateAroundZ False)
          , MuscleAxisConfig LeftForearmTwistInOut NoRotation
          ]
      , bmcIsRight = False
      }
  ]

-- | Find bone configuration from the database
findBoneConfig :: HumanoidBone -> Maybe BoneMuscleConfig
findBoneConfig bone = go boneMuscleDatabase
  where
    go [] = Nothing
    go (c:cs)
      | bmcBone c == bone = Just c
      | otherwise = go cs

-- | Apply signFlip adjustments from boneMuscleDatabase to euler angles
-- This ensures left/right symmetry for paired bones
applySignFlips :: HumanoidBone -> Float -> Float -> Float -> (Float, Float, Float)
applySignFlips bone rotX rotY rotZ =
  case findBoneConfig bone of
    Nothing -> (rotX, rotY, rotZ)  -- No config, no flips
    Just config ->
      let flipX = getSignFlip RotateAroundX (bmcAxes config)
          flipY = getSignFlip RotateAroundY (bmcAxes config)
          flipZ = getSignFlip RotateAroundZ (bmcAxes config)
      in (if flipX then -rotX else rotX,
          if flipY then -rotY else rotY,
          if flipZ then -rotZ else rotZ)
  where
    -- Check if any axis config has signFlip=True for the given axis type
    getSignFlip :: (Bool -> RotationAxis) -> [MuscleAxisConfig] -> Bool
    getSignFlip axisCtor configs = any (matchesAxisWithFlip axisCtor) configs

    matchesAxisWithFlip :: (Bool -> RotationAxis) -> MuscleAxisConfig -> Bool
    matchesAxisWithFlip axisCtor cfg = case macRotationAxis cfg of
      RotateAroundX True -> case axisCtor True of RotateAroundX _ -> True; _ -> False
      RotateAroundY True -> case axisCtor True of RotateAroundY _ -> True; _ -> False
      RotateAroundZ True -> case axisCtor True of RotateAroundZ _ -> True; _ -> False
      _ -> False

-- | Convert a quaternion rotation to muscle values for a bone
-- Returns a list of (MuscleId, MuscleValue) pairs
-- The rotation is computed relative to the Fetus Position reference rotation.
-- In Unity Humanoid, muscle value 0 = Fetus Position
-- The delta rotation from Fetus Position is converted to bone-local Euler angles,
-- then mapped to muscle values.
--
-- Unity Humanoid Axis Conventions (left-handed coordinate system):
--   DoF 0 = X-axis: Twist / Roll
--   DoF 1 = Y-axis: Side / Tilt / In-Out
--   DoF 2 = Z-axis: Front-Back / Stretch / Nod / Down-Up
--
-- Bone-specific axis meanings:
--   Spine/Chest: X=Twist, Y=Left-Right, Z=Front-Back
--   Neck/Head: X=Turn, Y=Tilt, Z=Nod
--   UpperArm: X=Twist, Y=Front-Back, Z=Down-Up
--   LowerArm: X=Twist, Z=Stretch (no Y)
--   UpperLeg: X=Twist, Y=In-Out, Z=Front-Back
--   LowerLeg: X=Twist, Z=Stretch (no Y)
quaternionToMuscles :: HumanoidBone -> Quaternion Float -> [(MuscleId, MuscleValue)]
quaternionToMuscles bone quat =
  let muscles = boneToMuscles bone
      -- Get Fetus Position reference rotation and compute delta
      fetusQ = fetusRotation bone
      -- deltaQ = currentQ * inverse(fetusQ) gives rotation from Fetus Position to current
      deltaQ = quaternionMul quat (quaternionInverse fetusQ)
      -- Get the muscle axes for this bone in Fetus Pose
      axes = fetusPoseMuscleAxes bone
      -- Extract rotation angles around the bone's local axes
      -- quaternionToAxisAngles returns angles around the bone's specific axes:
      --   rotX = rotation around X-axis (Twist)
      --   rotY = rotation around Y-axis (Spread/In-Out)
      --   rotZ = rotation around Z-axis (Stretch/Front-Back/Down-Up)
      (rotX, rotY, rotZ) = quaternionToAxisAngles axes deltaQ

      -- Apply signFlip from boneMuscleDatabase for left/right symmetry
      (adjRotX, adjRotY, adjRotZ) = applySignFlips bone rotX rotY rotZ

  in case bone of
    -- Spine bones: [Front-Back, Left-Right, Twist] = [Z, Y, X]
    Spine -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    Chest -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    UpperChest -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]

    -- Neck and Head: [Nod, Tilt, Turn] = [Z, Y, X]
    Neck -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    Head -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    Jaw -> zipMuscles muscles [adjRotZ, adjRotY]

    -- Eyes: [Down-Up, In-Out] = [Z, Y]
    LeftEye -> zipMuscles muscles [adjRotZ, adjRotY]
    RightEye -> zipMuscles muscles [adjRotZ, adjRotY]

    -- Shoulders: [Down-Up, Front-Back] = [Z, Y]
    LeftShoulder -> zipMuscles muscles [adjRotZ, adjRotY]
    RightShoulder -> zipMuscles muscles [adjRotZ, adjRotY]

    -- Upper Arm: [Down-Up, Front-Back, Twist] = [Z, Y, X]
    LeftUpperArm -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    RightUpperArm -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]

    -- Lower Arm (Forearm): [Stretch, Twist] = [Z, X]
    LeftLowerArm -> zipMuscles muscles [adjRotZ, adjRotX]
    RightLowerArm -> zipMuscles muscles [adjRotZ, adjRotX]

    -- Hand: [Down-Up, In-Out] = [Z, Y]
    LeftHand -> zipMuscles muscles [adjRotZ, adjRotY]
    RightHand -> zipMuscles muscles [adjRotZ, adjRotY]

    -- Upper Leg: [Front-Back, In-Out, Twist] = [Z, Y, X]
    LeftUpperLeg -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]
    RightUpperLeg -> zipMuscles muscles [adjRotZ, adjRotY, adjRotX]

    -- Lower Leg: [Stretch, Twist] = [Z, X]
    LeftLowerLeg -> zipMuscles muscles [adjRotZ, adjRotX]
    RightLowerLeg -> zipMuscles muscles [adjRotZ, adjRotX]

    -- Foot: [Up-Down, Twist] = [Z, X]
    LeftFoot -> zipMuscles muscles [adjRotZ, adjRotX]
    RightFoot -> zipMuscles muscles [adjRotZ, adjRotX]

    -- Toes: [Up-Down] = [Z]
    LeftToes -> zipMuscles muscles [adjRotZ]
    RightToes -> zipMuscles muscles [adjRotZ]

    -- Fingers: use Z-axis (stretch/curl)
    _ -> zipMuscles muscles (replicate (length muscles) adjRotZ)
  where
    zipMuscles :: [MuscleId] -> [Float] -> [(MuscleId, MuscleValue)]
    zipMuscles mids angles = zipWith toMuscle mids (angles ++ repeat 0)

    toMuscle :: MuscleId -> Float -> (MuscleId, MuscleValue)
    toMuscle mid angle =
      let MuscleRange minA maxA = muscleDefaultRange mid
          -- The input angle is already relative to Fetus Position (deltaQ),
          -- so no need to subtract muscleCenter.
          -- Simply normalize by the range:
          --   if angle >= 0: muscle = angle / max
          --   else:          muscle = angle / (-min)
          normalized = if angle >= 0
                       then clampF 0 1 (angle / maxA)
                       else clampF (-1) 0 (angle / (-minA))
      in (mid, normalized)

-- | Convert muscle values back to quaternion
musclesToQuaternion :: HumanoidBone -> [(MuscleId, MuscleValue)] -> Quaternion Float
musclesToQuaternion bone muscleValues =
  let muscleMap = Map.fromList muscleValues
      getAngle mid = case Map.lookup mid muscleMap of
        Just v ->
          let MuscleRange minA maxA = muscleDefaultRange mid
              range = maxA - minA
              center = (maxA + minA) / 2
          in center + v * (range / 2)
        Nothing -> 0
  in case bone of
    -- Spine bones
    Spine -> eulerToQuaternion (getAngle SpineFrontBack) (getAngle SpineTwistLeftRight) (getAngle SpineLeftRight)
    Chest -> eulerToQuaternion (getAngle ChestFrontBack) (getAngle ChestTwistLeftRight) (getAngle ChestLeftRight)
    UpperChest -> eulerToQuaternion (getAngle UpperChestFrontBack) (getAngle UpperChestTwistLeftRight) (getAngle UpperChestLeftRight)

    -- Default: identity
    _ -> Quaternion 1 (V3 0 0 0)

-- | Convert Euler angles (pitch, yaw, roll) in degrees to quaternion
eulerToQuaternion :: Float -> Float -> Float -> Quaternion Float
eulerToQuaternion pitchDeg yawDeg rollDeg =
  let pitch = toRadians pitchDeg
      yaw = toRadians yawDeg
      roll = toRadians rollDeg

      cy = cos (yaw * 0.5)
      sy = sin (yaw * 0.5)
      cp = cos (pitch * 0.5)
      sp = sin (pitch * 0.5)
      cr = cos (roll * 0.5)
      sr = sin (roll * 0.5)

      w = cr * cp * cy + sr * sp * sy
      x = sr * cp * cy - cr * sp * sy
      y = cr * sp * cy + sr * cp * sy
      z = cr * cp * sy - sr * sp * cy

  in Quaternion w (V3 x y z)

-- | Convert degrees to radians
toRadians :: Float -> Float
toRadians deg = deg * pi / 180

-- | Clamp a value between min and max
clampF :: Float -> Float -> Float -> Float
clampF lo hi x = max lo (min hi x)

-- | Quaternion inverse (conjugate for unit quaternions)
quaternionInverse :: Quaternion Float -> Quaternion Float
quaternionInverse (Quaternion w (V3 x y z)) = Quaternion w (V3 (-x) (-y) (-z))

-- | Quaternion multiplication
quaternionMul :: Quaternion Float -> Quaternion Float -> Quaternion Float
quaternionMul (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  Quaternion (w1*w2 - x1*x2 - y1*y2 - z1*z2)
             (V3 (w1*x2 + x1*w2 + y1*z2 - z1*y2)
                 (w1*y2 - x1*z2 + y1*w2 + z1*x2)
                 (w1*z2 + x1*y2 - y1*x2 + z1*w2))

-- | Fetus Position reference rotations computed from the default skeleton
-- This uses the exact same computation as Animation.hs uses for animation frames
-- In Unity Humanoid, muscle value 0 = Fetus Position
fetusRotations :: Map.Map HumanoidBone (Quaternion Float)
fetusRotations =
  let skeleton = buildSkeleton defaultSkeletonConfig
      positions = Map.map transformPosition (skeletonRestPose skeleton)
      fullPose = computeRotations skeleton positions Map.empty
  in Map.map transformRotation fullPose

-- | Get Fetus Position rotation for a specific bone
fetusRotation :: HumanoidBone -> Quaternion Float
fetusRotation bone = Map.findWithDefault identityQ bone fetusRotations
  where
    identityQ = Quaternion 1 (V3 0 0 0)

-- | Fetus Position reference positions computed from the default skeleton
fetusPositions :: Map.Map HumanoidBone (V3 Float)
fetusPositions =
  let skeleton = buildSkeleton defaultSkeletonConfig
  in Map.map transformPosition (skeletonRestPose skeleton)

-- | Get Fetus Position direction for a bone (from parent to child)
--
-- For bones like RightUpperArm, this returns the direction from shoulder to elbow
-- in Fetus Position. This is used as the reference for muscle calculations.
fetusDirection :: HumanoidBone -> V3 Float
fetusDirection bone =
  let positions = fetusPositions
      childBone = getChildForMuscle bone
  in case (Map.lookup bone positions, Map.lookup childBone positions) of
       (Just parentPos, Just childPos) -> childPos - parentPos
       _ -> V3 0 (-1) 0  -- Default: pointing down
  where
    -- Get the child bone used for muscle direction calculation
    getChildForMuscle :: HumanoidBone -> HumanoidBone
    getChildForMuscle b = case b of
      RightUpperArm -> RightLowerArm
      LeftUpperArm  -> LeftLowerArm
      RightUpperLeg -> RightLowerLeg
      LeftUpperLeg  -> LeftLowerLeg
      RightLowerArm -> RightHand
      LeftLowerArm  -> LeftHand
      RightLowerLeg -> RightFoot
      LeftLowerLeg  -> LeftFoot
      _ -> b  -- Default to self

-- | Convert bone positions directly to muscle values
--
-- Unity Humanoid Muscle specification:
--   - Muscle value is measured in the bone's LOCAL coordinate system
--   - The local coordinate system is defined by the PARENT bone's current rotation
--   - When parent rotates, the local coordinate system rotates with it
--   - Muscle 0 = Fetus pose direction (reference)
--   - Z-axis rotation (Down-Up) = rotation in XY plane of local coords
--
-- Algorithm:
--   1. Compute current direction in global coordinates (child - parent)
--   2. Transform both current and Fetus directions to parent's local coordinate system
--   3. Compute angle difference between them in the appropriate plane
--   4. Map angle to Muscle value [-1, 1]
positionToMuscles
  :: HumanoidBone           -- ^ Target bone (e.g., RightUpperArm)
  -> V3 Float               -- ^ Parent bone position (global coords)
  -> V3 Float               -- ^ Child bone position (global coords)
  -> V3 Float               -- ^ Fetus direction (global coords, reference)
  -> Quaternion Float       -- ^ Parent bone's current rotation (defines local coord system)
  -> [(MuscleId, MuscleValue)]
positionToMuscles bone parentPos childPos fetusDir parentRot =
  case findBoneConfig bone of
    Nothing -> []
    Just config ->
      let -- Current direction in global coordinates
          currentDirGlobal = childPos - parentPos

          -- Transform both directions to parent's local coordinate system
          -- Using inverse of parent rotation transforms global -> local
          currentDirLocal = rotateByInverse parentRot currentDirGlobal
          fetusDirLocal = rotateByInverse parentRot fetusDir

      in computeMusclesFromConfig config currentDirLocal fetusDirLocal

-- | Rotate a vector by the inverse of a quaternion
-- This transforms from global to local coordinates
rotateByInverse :: Quaternion Float -> V3 Float -> V3 Float
rotateByInverse q v = rotateV (conjugate q) v
  where
    conjugate (Quaternion w (V3 x y z)) = Quaternion w (V3 (-x) (-y) (-z))

-- | Rotate a vector by a quaternion
-- Uses Rodrigues' rotation formula: v' = v + 2w(q × v) + 2(q × (q × v))
-- This is equivalent to q * v * q^(-1) but more efficient
rotateV :: Quaternion Float -> V3 Float -> V3 Float
rotateV (Quaternion w (V3 qx qy qz)) (V3 vx vy vz) =
  let -- t = 2 * cross(q.xyz, v)
      tx = 2 * (qy * vz - qz * vy)
      ty = 2 * (qz * vx - qx * vz)
      tz = 2 * (qx * vy - qy * vx)
      -- result = v + w * t + cross(q.xyz, t)
      rx = vx + w * tx + (qy * tz - qz * ty)
      ry = vy + w * ty + (qz * tx - qx * tz)
      rz = vz + w * tz + (qx * ty - qy * tx)
  in V3 rx ry rz

-- | Compute muscles from bone configuration
-- This is the unified function that handles all bone types
computeMusclesFromConfig
  :: BoneMuscleConfig
  -> V3 Float  -- ^ Current direction (parent to child)
  -> V3 Float  -- ^ Fetus position direction (reference)
  -> [(MuscleId, MuscleValue)]
computeMusclesFromConfig config dir fetusDir =
  map (computeAxisMuscle config dir fetusDir) (bmcAxes config)

-- | Compute muscle value for a single axis
computeAxisMuscle
  :: BoneMuscleConfig
  -> V3 Float  -- ^ Current direction
  -> V3 Float  -- ^ Fetus direction
  -> MuscleAxisConfig
  -> (MuscleId, MuscleValue)
computeAxisMuscle _config dir _fetusDir axisConfig =
  let muscleId = macMuscleId axisConfig
      rotAxis = macRotationAxis axisConfig

      -- Calculate angle based on rotation axis
      angle = computeAngleAroundAxis dir rotAxis

      -- Map angle to muscle value
      muscleValue = angleToMuscle muscleId angle

  in (muscleId, muscleValue)

-- | Compute angle around a specific axis
-- The angle is measured from the +X axis (child direction / Fetus pose reference)
-- in the plane perpendicular to the rotation axis
computeAngleAroundAxis :: V3 Float -> RotationAxis -> Float
computeAngleAroundAxis dir rotAxis =
  let refDir = V3 1 0 0  -- Local X axis = child direction = Fetus pose reference
  in case rotAxis of
       RotateAroundX signFlip ->
         -- X-axis rotation: angle in YZ plane
         let angle = signedAngleBetween refDir dir (V3 1 0 0)
         in if signFlip then -angle else angle
       RotateAroundY signFlip ->
         -- Y-axis rotation: angle in XZ plane
         let angle = signedAngleBetween refDir dir (V3 0 1 0)
         in if signFlip then -angle else angle
       RotateAroundZ signFlip ->
         -- Z-axis rotation: angle in XY plane
         let angle = signedAngleBetween refDir dir (V3 0 0 1)
         in if signFlip then -angle else angle
       NoRotation -> 0  -- Cannot determine from position

-- | Calculate signed angle between two vectors around a specified axis
-- Uses cross product to determine sign
signedAngleBetween :: V3 Float -> V3 Float -> V3 Float -> Float
signedAngleBetween from to axis =
  let -- Normalize vectors
      fromN = normalizeV from
      toN = normalizeV to
      axisN = normalizeV axis
      -- Angle magnitude via dot product
      dotP = dotV fromN toN
      angleMag = acos (max (-1) (min 1 dotP)) * 180 / pi
      -- Sign via cross product dot with axis
      crossP = crossV fromN toN
      sign = if dotV crossP axisN >= 0 then 1 else (-1)
  in sign * angleMag

-- | Normalize a vector
normalizeV :: V3 Float -> V3 Float
normalizeV v@(V3 x y z) =
  let m = sqrt (x*x + y*y + z*z)
  in if m > 0.0001 then V3 (x/m) (y/m) (z/m) else v

-- | Dot product of two vectors
dotV :: V3 Float -> V3 Float -> Float
dotV (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | Cross product of two vectors
crossV :: V3 Float -> V3 Float -> V3 Float
crossV (V3 x1 y1 z1) (V3 x2 y2 z2) =
  V3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

-- | Convert angle (in degrees) to muscle value
--
-- The input angleDeg is the absolute angle in the bone's local coordinate system.
-- The muscle value is calculated relative to the center angle.
--
-- From unity_humanoid_center_of_mass.md:
--   if angle >= center:
--       muscle = (angle - center) / max
--   else:
--       muscle = (angle - center) / (-min)
--
-- Examples:
--   Right Arm Down-Up: center=-40, min=-60, max=100
--     angle=-40 (Fetus) -> muscle = 0
--     angle=60 (arm up) -> muscle = (60 - (-40)) / 100 = 1.0
--     angle=-100 (arm down) -> muscle = (-100 - (-40)) / 60 = -1.0
angleToMuscle :: MuscleId -> Float -> MuscleValue
angleToMuscle muscleId angleDeg =
  let MuscleRange minA maxA = muscleDefaultRange muscleId
      center = muscleCenter muscleId
      relativeAngle = angleDeg - center
  in if relativeAngle >= 0
     then clampF 0 1 (relativeAngle / maxA)
     else clampF (-1) 0 (relativeAngle / (-minA))

-- ============================================================================
-- Forward Kinematics: Muscle values -> Bone positions
-- ============================================================================

-- | Convert muscle values to bone positions (Forward Kinematics)
-- Takes a map of muscle name -> value and returns bone positions
--
-- Algorithm:
--   1. Start from Fetus pose (base positions and rotations)
--   2. For each bone, compute local rotation from muscle values
--   3. Propagate rotations down the hierarchy (parent rotation affects child position)
--   4. Apply rotations to compute world positions
musclesToPose
  :: Map.Map String MuscleValue  -- ^ Muscle values by name
  -> Map.Map HumanoidBone (V3 Float)  -- ^ Bone positions
musclesToPose muscleMap =
  let -- Get base Fetus positions
      basePositions = fetusPositions

      -- Propagate rotations down the hierarchy to get world positions
      -- Process bones in depth order (parents before children)
      bonesByDepth = sortBy (comparing getDepth) allBones

      -- Accumulate world rotations and positions
      (worldPositions, _worldRotations) =
        foldl propagateBone (basePositions, Map.empty) bonesByDepth

  in worldPositions
  where
    -- Propagate rotation to a bone, computing its world position
    propagateBone
      :: (Map.Map HumanoidBone (V3 Float), Map.Map HumanoidBone (Quaternion Float))
      -> HumanoidBone
      -> (Map.Map HumanoidBone (V3 Float), Map.Map HumanoidBone (Quaternion Float))
    propagateBone (positions, rotations) bone =
      let -- Get local rotation for this bone
          localRot = Map.findWithDefault identityQ bone
                       (Map.fromList [(b, musclesToLocalRotation b muscleMap) | b <- allBones])

          -- Get parent's world rotation (identity if no parent)
          parentWorldRot = case boneParent bone of
            Nothing -> identityQ
            Just parent -> Map.findWithDefault identityQ parent rotations

          -- World rotation = parent rotation * local rotation
          worldRot = quaternionMul parentWorldRot localRot

          -- Get the offset from parent in Fetus pose
          (parentFetusPos, fetusOffset) = case boneParent bone of
            Nothing -> (V3 0 0 0, Map.findWithDefault (V3 0 0 0) bone fetusPositions)
            Just parent ->
              let pPos = Map.findWithDefault (V3 0 0 0) parent fetusPositions
                  bPos = Map.findWithDefault (V3 0 0 0) bone fetusPositions
              in (pPos, bPos - pPos)

          -- Get parent's current world position
          parentWorldPos = case boneParent bone of
            Nothing -> V3 0 0 0
            Just parent -> Map.findWithDefault parentFetusPos parent positions

          -- Rotate the offset by parent's world rotation and add to parent position
          rotatedOffset = rotateV parentWorldRot fetusOffset
          worldPos = parentWorldPos + rotatedOffset

      in (Map.insert bone worldPos positions, Map.insert bone worldRot rotations)

    identityQ = Quaternion 1 (V3 0 0 0)

-- | Convert muscle values to bone Transforms (position + rotation)
-- This is the complete FK function that returns both position and rotation.
-- Unlike musclesToPose, this preserves the rotation information computed from muscles.
--
-- The rotation is computed as: fetusRotation(bone) * localRotation
-- where localRotation is the rotation from muscle values.
-- This produces a world rotation that quaternionToMuscles can invert correctly.
musclesToTransforms
  :: Map.Map String MuscleValue  -- ^ Muscle values by name
  -> Map.Map HumanoidBone Transform  -- ^ Bone transforms (position + rotation)
musclesToTransforms muscleMap =
  let -- Get base Fetus positions
      basePositions = fetusPositions

      -- Propagate rotations down the hierarchy to get world positions
      -- Process bones in depth order (parents before children)
      bonesByDepth = sortBy (comparing getDepth) allBones

      -- Accumulate world rotations and positions
      (worldPositions, worldRotations) =
        foldl propagateBone (basePositions, Map.empty) bonesByDepth

  in Map.mapWithKey (\bone pos ->
       let rot = Map.findWithDefault identityQ bone worldRotations
       in Transform pos rot) worldPositions
  where
    -- Propagate rotation to a bone, computing its world position
    propagateBone
      :: (Map.Map HumanoidBone (V3 Float), Map.Map HumanoidBone (Quaternion Float))
      -> HumanoidBone
      -> (Map.Map HumanoidBone (V3 Float), Map.Map HumanoidBone (Quaternion Float))
    propagateBone (positions, rotations) bone =
      let -- Get local rotation delta for this bone (from muscle values)
          localRotDelta = Map.findWithDefault identityQ bone
                       (Map.fromList [(b, musclesToLocalRotation b muscleMap) | b <- allBones])

          -- Get Fetus pose rotation for this bone
          fetusQ = fetusRotation bone

          -- World rotation = localRotDelta * fetusRotation
          -- quaternionToMuscles computes: deltaQ = quat * inverse(fetusQ)
          -- With worldRot = localRotDelta * fetusQ:
          --   deltaQ = localRotDelta * fetusQ * inverse(fetusQ) = localRotDelta
          -- This gives us back the local rotation delta, which is what we want
          worldRot = quaternionMul localRotDelta fetusQ

          -- Get parent's world rotation for position calculation
          parentWorldRot = case boneParent bone of
            Nothing -> identityQ
            Just parent -> Map.findWithDefault identityQ parent rotations

          -- Get the offset from parent in Fetus pose
          (parentFetusPos, fetusOffset) = case boneParent bone of
            Nothing -> (V3 0 0 0, Map.findWithDefault (V3 0 0 0) bone fetusPositions)
            Just parent ->
              let pPos = Map.findWithDefault (V3 0 0 0) parent fetusPositions
                  bPos = Map.findWithDefault (V3 0 0 0) bone fetusPositions
              in (pPos, bPos - pPos)

          -- Get parent's current world position
          parentWorldPos = case boneParent bone of
            Nothing -> V3 0 0 0
            Just parent -> Map.findWithDefault parentFetusPos parent positions

          -- Rotate the offset by parent's world rotation and add to parent position
          rotatedOffset = rotateV parentWorldRot fetusOffset
          worldPos = parentWorldPos + rotatedOffset

      in (Map.insert bone worldPos positions, Map.insert bone worldRot rotations)

    identityQ = Quaternion 1 (V3 0 0 0)

-- | Convert muscle values to local rotation for a bone
-- This is the inverse of quaternionToMuscles.
-- Uses fetusPoseMuscleAxes to ensure round-trip consistency.
musclesToLocalRotation :: HumanoidBone -> Map.Map String MuscleValue -> Quaternion Float
musclesToLocalRotation bone muscleMap =
  let muscles = boneToMuscles bone
      axes = fetusPoseMuscleAxes bone

      -- Get muscle value by property name
      getMuscle :: MuscleId -> Float
      getMuscle mid = fromMaybe 0 $ Map.lookup (musclePropertyName mid) muscleMap

      -- Convert muscle value to angle (degrees)
      -- This is the inverse of what quaternionToMuscles.toMuscle does
      muscleToAngle :: MuscleId -> Float
      muscleToAngle mid =
        let value = getMuscle mid
            MuscleRange minA maxA = muscleDefaultRange mid
            -- The input value is normalized:
            --   if value >= 0: value = angle / max
            --   else:          value = angle / (-min)
            -- So the inverse is:
            --   if value >= 0: angle = value * max
            --   else:          angle = value * (-min)
        in if value >= 0
           then value * maxA
           else value * (-minA)

      -- Apply inverse sign flips
      -- Since sign flipping is its own inverse (flip(flip(x)) = x), we use applySignFlips directly
      applyInverseSignFlips = applySignFlips

      -- Get angles for each axis based on bone type
      -- This must match the structure in quaternionToMuscles
      (rawRotX, rawRotY, rawRotZ) = case bone of
        -- Spine bones: [Front-Back, Left-Right, Twist] -> [Z, Y, X]
        Spine -> case muscles of
          [fb, lr, tw] -> (muscleToAngle tw, muscleToAngle lr, muscleToAngle fb)
          _ -> (0, 0, 0)
        Chest -> case muscles of
          [fb, lr, tw] -> (muscleToAngle tw, muscleToAngle lr, muscleToAngle fb)
          _ -> (0, 0, 0)
        UpperChest -> case muscles of
          [fb, lr, tw] -> (muscleToAngle tw, muscleToAngle lr, muscleToAngle fb)
          _ -> (0, 0, 0)

        -- Neck and Head: [Nod, Tilt, Turn] -> [Z, Y, X]
        Neck -> case muscles of
          [nod, tilt, turn] -> (muscleToAngle turn, muscleToAngle tilt, muscleToAngle nod)
          _ -> (0, 0, 0)
        Head -> case muscles of
          [nod, tilt, turn] -> (muscleToAngle turn, muscleToAngle tilt, muscleToAngle nod)
          _ -> (0, 0, 0)

        -- Shoulders: [Down-Up, Front-Back] -> [Z, Y]
        LeftShoulder -> case muscles of
          [du, fb] -> (0, muscleToAngle fb, muscleToAngle du)
          _ -> (0, 0, 0)
        RightShoulder -> case muscles of
          [du, fb] -> (0, muscleToAngle fb, muscleToAngle du)
          _ -> (0, 0, 0)

        -- Upper Arm: [Down-Up, Front-Back, Twist] -> [Z, Y, X]
        LeftUpperArm -> case muscles of
          [du, fb, tw] -> (muscleToAngle tw, muscleToAngle fb, muscleToAngle du)
          _ -> (0, 0, 0)
        RightUpperArm -> case muscles of
          [du, fb, tw] -> (muscleToAngle tw, muscleToAngle fb, muscleToAngle du)
          _ -> (0, 0, 0)

        -- Lower Arm: [Stretch, Twist] -> [Z, X]
        LeftLowerArm -> case muscles of
          [st, tw] -> (muscleToAngle tw, 0, muscleToAngle st)
          _ -> (0, 0, 0)
        RightLowerArm -> case muscles of
          [st, tw] -> (muscleToAngle tw, 0, muscleToAngle st)
          _ -> (0, 0, 0)

        -- Hand: [Down-Up, In-Out] -> [Z, Y]
        LeftHand -> case muscles of
          [du, io] -> (0, muscleToAngle io, muscleToAngle du)
          _ -> (0, 0, 0)
        RightHand -> case muscles of
          [du, io] -> (0, muscleToAngle io, muscleToAngle du)
          _ -> (0, 0, 0)

        -- Upper Leg: [Front-Back, In-Out, Twist] -> [Z, Y, X]
        LeftUpperLeg -> case muscles of
          [fb, io, tw] -> (muscleToAngle tw, muscleToAngle io, muscleToAngle fb)
          _ -> (0, 0, 0)
        RightUpperLeg -> case muscles of
          [fb, io, tw] -> (muscleToAngle tw, muscleToAngle io, muscleToAngle fb)
          _ -> (0, 0, 0)

        -- Lower Leg: [Stretch, Twist] -> [Z, X]
        LeftLowerLeg -> case muscles of
          [st, tw] -> (muscleToAngle tw, 0, muscleToAngle st)
          _ -> (0, 0, 0)
        RightLowerLeg -> case muscles of
          [st, tw] -> (muscleToAngle tw, 0, muscleToAngle st)
          _ -> (0, 0, 0)

        -- Foot: [Up-Down, Twist] -> [Z, X]
        LeftFoot -> case muscles of
          [ud, tw] -> (muscleToAngle tw, 0, muscleToAngle ud)
          _ -> (0, 0, 0)
        RightFoot -> case muscles of
          [ud, tw] -> (muscleToAngle tw, 0, muscleToAngle ud)
          _ -> (0, 0, 0)

        -- Default: no rotation
        _ -> (0, 0, 0)

      -- Apply inverse sign flips
      (rotX, rotY, rotZ) = applyInverseSignFlips bone rawRotX rawRotY rawRotZ

      -- Convert to radians
      toRad deg = deg * pi / 180

      -- Create rotation around each axis using fetusPoseMuscleAxes
      rotAroundX = quaternionFromAxisAngle (axisX axes) (toRad rotX)
      rotAroundY = quaternionFromAxisAngle (axisY axes) (toRad rotY)
      rotAroundZ = quaternionFromAxisAngle (axisZ axes) (toRad rotZ)

  -- Combine rotations: Z * Y * X (to match quaternionToAxisAngles extraction order)
  in quaternionMul rotAroundZ (quaternionMul rotAroundY rotAroundX)

-- | Sample muscles at a time and convert to bone positions
-- This is a convenience function for animation retargeting
sampleMusclesToPose
  :: [(String, [(Float, MuscleValue)])]  -- ^ Muscle curves: [(name, [(time, value)])]
  -> Float                                -- ^ Time to sample
  -> Map.Map HumanoidBone (V3 Float)      -- ^ Bone positions
sampleMusclesToPose curves time =
  let -- Sample each muscle at the given time
      sampleCurve :: [(Float, MuscleValue)] -> MuscleValue
      sampleCurve [] = 0
      sampleCurve [(_, v)] = v
      sampleCurve kfs =
        let sorted = sortBy (comparing fst) kfs
            before = filter (\(t, _) -> t <= time) sorted
            after = filter (\(t, _) -> t > time) sorted
        in case (before, after) of
          ([], (_, v):_) -> v
          (b, []) -> snd (last b)
          (b, (t2, v2):_) ->
            let (t1, v1) = last b
                ratio = if t2 == t1 then 0 else (time - t1) / (t2 - t1)
            in v1 + ratio * (v2 - v1)

      muscleMap = Map.fromList [(name, sampleCurve kfs) | (name, kfs) <- curves]
  in musclesToPose muscleMap
