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
  , MeasurementPlane(..)
  , boneMuscleDatabase
  , findBoneConfig

    -- * Conversion
  , quaternionToMuscles
  , musclesToQuaternion
  , boneToMuscles
  , positionToMuscles
  , fetusDirection
  , fetusRotation
  ) where

import qualified Data.Map.Strict as Map
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Skeleton.Bones (HumanoidBone(..))
import HumanoidAnim.Skeleton.Config (defaultSkeletonConfig, buildSkeleton, Transform(..), Skeleton(..))
import HumanoidAnim.Skeleton.Transform (computeRotations)

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

-- | Measurement plane for angle calculation
-- Each plane defines how to project the bone direction to calculate angles
data MeasurementPlane
  = SagittalPlane           -- ^ YZ plane - measures Front-Back angles
  | FrontalPlane Bool       -- ^ XY plane - measures In-Out / Left-Right angles. Bool = signFlip
  | TwistAxis               -- ^ Rotation around bone axis (cannot be determined from position)
  deriving stock (Show, Eq)

-- | Configuration for a single muscle axis
data MuscleAxisConfig = MuscleAxisConfig
  { macMuscleId :: MuscleId           -- ^ The muscle ID for this axis
  , macPlane :: MeasurementPlane      -- ^ Which plane to measure angle in
  } deriving stock (Show, Eq)

-- | Complete muscle configuration for a bone
data BoneMuscleConfig = BoneMuscleConfig
  { bmcBone :: HumanoidBone           -- ^ The bone this config applies to
  , bmcAxes :: [MuscleAxisConfig]     -- ^ Configuration for each muscle axis
  , bmcIsRight :: Bool                -- ^ Whether this is a right-side bone
  } deriving stock (Show, Eq)

-- | Database of bone muscle configurations
-- This centralizes all the bone-specific logic for position-based muscle calculation
boneMuscleDatabase :: [BoneMuscleConfig]
boneMuscleDatabase =
  -- Upper Legs
  -- In-Out: 内側(-1) ↔ 外側(+1)
  -- Right leg: outward = +X, so no flip needed
  -- Left leg: outward = -X, so flip needed to make outward = positive
  [ BoneMuscleConfig
      { bmcBone = RightUpperLeg
      , bmcAxes =
          [ MuscleAxisConfig RightUpperLegFrontBack SagittalPlane
          , MuscleAxisConfig RightUpperLegInOut (FrontalPlane False)  -- Right: no flip, +X = outward = positive
          , MuscleAxisConfig RightUpperLegTwistInOut TwistAxis
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftUpperLeg
      , bmcAxes =
          [ MuscleAxisConfig LeftUpperLegFrontBack SagittalPlane
          , MuscleAxisConfig LeftUpperLegInOut (FrontalPlane True)   -- Left: flip, -X = outward, flip to positive
          , MuscleAxisConfig LeftUpperLegTwistInOut TwistAxis
          ]
      , bmcIsRight = False
      }

  -- Upper Arms
  , BoneMuscleConfig
      { bmcBone = RightUpperArm
      , bmcAxes =
          [ MuscleAxisConfig RightArmDownUp (FrontalPlane False)
          , MuscleAxisConfig RightArmFrontBack SagittalPlane
          , MuscleAxisConfig RightArmTwistInOut TwistAxis
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftUpperArm
      , bmcAxes =
          [ MuscleAxisConfig LeftArmDownUp (FrontalPlane False)
          , MuscleAxisConfig LeftArmFrontBack SagittalPlane
          , MuscleAxisConfig LeftArmTwistInOut TwistAxis
          ]
      , bmcIsRight = False
      }

  -- Lower Legs
  , BoneMuscleConfig
      { bmcBone = RightLowerLeg
      , bmcAxes =
          [ MuscleAxisConfig RightLegStretch SagittalPlane
          , MuscleAxisConfig RightLegTwistInOut TwistAxis
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftLowerLeg
      , bmcAxes =
          [ MuscleAxisConfig LeftLegStretch SagittalPlane
          , MuscleAxisConfig LeftLegTwistInOut TwistAxis
          ]
      , bmcIsRight = False
      }

  -- Lower Arms (Forearms)
  , BoneMuscleConfig
      { bmcBone = RightLowerArm
      , bmcAxes =
          [ MuscleAxisConfig RightForearmStretch SagittalPlane
          , MuscleAxisConfig RightForearmTwistInOut TwistAxis
          ]
      , bmcIsRight = True
      }
  , BoneMuscleConfig
      { bmcBone = LeftLowerArm
      , bmcAxes =
          [ MuscleAxisConfig LeftForearmStretch SagittalPlane
          , MuscleAxisConfig LeftForearmTwistInOut TwistAxis
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
      -- Extract euler angles from the delta quaternion
      -- quaternionToEulerXYZ returns angles in Unity convention:
      --   rotX = rotation around X-axis (Twist)
      --   rotY = rotation around Y-axis (Side/In-Out)
      --   rotZ = rotation around Z-axis (Front-Back/Stretch)
      (rotX, rotY, rotZ) = quaternionToEulerXYZ deltaQ

  in case bone of
    -- Spine bones: [Front-Back, Left-Right, Twist] = [Z, Y, X]
    Spine -> zipMuscles muscles [rotZ, rotY, rotX]
    Chest -> zipMuscles muscles [rotZ, rotY, rotX]
    UpperChest -> zipMuscles muscles [rotZ, rotY, rotX]

    -- Neck and Head: [Nod, Tilt, Turn] = [Z, Y, X]
    Neck -> zipMuscles muscles [rotZ, rotY, rotX]
    Head -> zipMuscles muscles [rotZ, rotY, rotX]
    Jaw -> zipMuscles muscles [rotZ, rotY]

    -- Eyes: [Down-Up, In-Out] = [Z, Y]
    LeftEye -> zipMuscles muscles [rotZ, rotY]
    RightEye -> zipMuscles muscles [rotZ, rotY]

    -- Shoulders: [Down-Up, Front-Back] = [Z, Y]
    LeftShoulder -> zipMuscles muscles [rotZ, rotY]
    RightShoulder -> zipMuscles muscles [rotZ, rotY]

    -- Upper Arm: [Down-Up, Front-Back, Twist] = [Z, Y, X]
    -- Note: Right side may need sign flip for symmetry
    LeftUpperArm -> zipMuscles muscles [rotZ, rotY, rotX]
    RightUpperArm -> zipMuscles muscles [rotZ, rotY, rotX]

    -- Lower Arm (Forearm): [Stretch, Twist] = [Z, X]
    -- Stretch range: -80° ~ 5° (negative = bent, positive = extended)
    LeftLowerArm -> zipMuscles muscles [rotZ, rotX]
    RightLowerArm -> zipMuscles muscles [rotZ, rotX]

    -- Hand: [Down-Up, In-Out] = [Z, Y]
    LeftHand -> zipMuscles muscles [rotZ, rotY]
    RightHand -> zipMuscles muscles [rotZ, rotY]

    -- Upper Leg: [Front-Back, In-Out, Twist] = [Z, Y, X]
    LeftUpperLeg -> zipMuscles muscles [rotZ, rotY, rotX]
    RightUpperLeg -> zipMuscles muscles [rotZ, rotY, rotX]

    -- Lower Leg: [Stretch, Twist] = [Z, X]
    -- Stretch range: -80° ~ 5°
    LeftLowerLeg -> zipMuscles muscles [rotZ, rotX]
    RightLowerLeg -> zipMuscles muscles [rotZ, rotX]

    -- Foot: [Up-Down, Twist] = [Z, X]
    LeftFoot -> zipMuscles muscles [rotZ, rotX]
    RightFoot -> zipMuscles muscles [rotZ, rotX]

    -- Toes: [Up-Down] = [Z]
    LeftToes -> zipMuscles muscles [rotZ]
    RightToes -> zipMuscles muscles [rotZ]

    -- Fingers: use Z-axis (stretch/curl)
    _ -> zipMuscles muscles (replicate (length muscles) rotZ)
  where
    zipMuscles :: [MuscleId] -> [Float] -> [(MuscleId, MuscleValue)]
    zipMuscles mids angles = zipWith toMuscle mids (angles ++ repeat 0)

    toMuscle :: MuscleId -> Float -> (MuscleId, MuscleValue)
    toMuscle mid angle =
      let MuscleRange minA maxA = muscleDefaultRange mid
          -- Convert angle (in degrees) to muscle value (-1 to 1)
          -- For "Stretch" muscles (min=0), use 0-to-1 range instead of -1-to-1
          normalized = if isStretchMuscle mid
                       then -- Stretch: 0 = min angle, 1 = max angle
                            let range = maxA - minA
                            in if range > 0
                               then clampF 0 1 ((angle - minA) / range)
                               else 0
                       else -- Normal: center = 0, map to -1..1
                            let range = maxA - minA
                                center = (maxA + minA) / 2
                            in if range > 0
                               then clampF (-1) 1 ((angle - center) / (range / 2))
                               else 0
      in (mid, normalized)

    -- Check if muscle is a "Stretch" type (uses 0-1 range instead of -1-1)
    isStretchMuscle :: MuscleId -> Bool
    isStretchMuscle m = case m of
      LeftForearmStretch -> True
      RightForearmStretch -> True
      LeftLegStretch -> True
      RightLegStretch -> True
      _ -> False

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

-- | Convert quaternion to Euler angles in XYZ order (Unity convention)
-- Returns (rotX, rotY, rotZ) in degrees
-- Unity uses left-handed coordinate system with XYZ intrinsic rotation order
quaternionToEulerXYZ :: Quaternion Float -> (Float, Float, Float)
quaternionToEulerXYZ (Quaternion w (V3 x y z)) =
  let -- Rotation around X-axis (Twist/Roll)
      sinX = 2 * (w * x - y * z)
      cosX = 1 - 2 * (x * x + z * z)
      rotX = atan2 sinX cosX

      -- Rotation around Y-axis (Side/Tilt)
      sinY = 2 * (w * y + x * z)
      rotY = if abs sinY >= 1
             then signum sinY * (pi / 2)
             else asin sinY

      -- Rotation around Z-axis (Front-Back/Stretch)
      sinZ = 2 * (w * z - x * y)
      cosZ = 1 - 2 * (y * y + z * z)
      rotZ = atan2 sinZ cosZ

  in (toDegrees rotX, toDegrees rotY, toDegrees rotZ)

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

-- | Convert radians to degrees
toDegrees :: Float -> Float
toDegrees rad = rad * 180 / pi

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
rotateV :: Quaternion Float -> V3 Float -> V3 Float
rotateV (Quaternion qw (V3 qx qy qz)) (V3 vx vy vz) =
  let -- q * v (treating v as quaternion with w=0)
      tw = -qx*vx - qy*vy - qz*vz
      tx =  qw*vx + qy*vz - qz*vy
      ty =  qw*vy + qz*vx - qx*vz
      tz =  qw*vz + qx*vy - qy*vx
      -- (q * v) * q^(-1) = (q * v) * conjugate(q)
      rx = -tw*(-qx) + tx*qw - ty*(-qz) + tz*(-qy)
      ry = -tw*(-qy) + ty*qw - tz*(-qx) + tx*(-qz)
      rz = -tw*(-qz) + tz*qw - tx*(-qy) + ty*(-qx)
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
computeAxisMuscle config dir fetusDir axisConfig =
  let muscleId = macMuscleId axisConfig
      plane = macPlane axisConfig
      isRight = bmcIsRight config

      -- Calculate angle based on measurement plane
      angle = case plane of
        SagittalPlane -> computeSagittalAngle dir fetusDir
        FrontalPlane signFlip -> computeFrontalAngle dir fetusDir isRight signFlip
        TwistAxis -> 0  -- Cannot determine from position

      -- Map angle to muscle value
      muscleValue = angleToMuscle muscleId angle

  in (muscleId, muscleValue)

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

-- | Compute angle in sagittal plane (YZ plane) relative to Fetus Position
-- Used for Front-Back and Stretch muscles
-- Returns the angle difference from Fetus direction
--
-- In local coordinates (after parent rotation is applied):
--   Y-axis = bone's "up" direction
--   Z-axis = bone's "forward" direction
--
-- Front-Back measures rotation around Y-axis (rotation in YZ plane)
computeSagittalAngle :: V3 Float -> V3 Float -> Float
computeSagittalAngle dir fetusDir =
  signedAngleBetween fetusDir dir (V3 1 0 0)  -- Rotation axis is X

-- | Compute angle in frontal plane (XY plane) relative to Fetus Position
-- Used for Down-Up muscles (arms and legs)
--
-- Unity specification (from docs):
--   - Down-Up is Z-axis rotation in the bone's local space
--   - Muscle -1 = Down / first word of muscle name
--   - Muscle +1 = Up / second word of muscle name
--   - Fetus pose = Muscle 0
--
-- In local coordinates:
--   Z-axis rotation = rotation in XY plane
--   We measure the angle from Fetus direction to current direction
--   projected onto the XY plane
--
-- Parameters:
--   signFlip: if True, negate the final angle (for left/right symmetry)
computeFrontalAngle :: V3 Float -> V3 Float -> Bool -> Bool -> Float
computeFrontalAngle dir fetusDir _isRight signFlip =
  -- Project onto XY plane (ignore Z component for Z-axis rotation)
  let V3 dx dy _ = dir
      V3 fdx fdy _ = fetusDir

      -- Angle of current direction in XY plane (from +X axis, counterclockwise)
      currentAngle = atan2 dy dx * 180 / pi

      -- Angle of Fetus direction in XY plane
      fetusAngle = atan2 fdy fdx * 180 / pi

      -- Relative angle (how much we rotated from Fetus)
      rawDelta = currentAngle - fetusAngle

      -- Normalize to [-180, 180]
      delta = if rawDelta > 180 then rawDelta - 360
              else if rawDelta < (-180) then rawDelta + 360
              else rawDelta

      -- Apply sign flip for left/right symmetry
      finalDelta = if signFlip then -delta else delta

  in finalDelta

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

