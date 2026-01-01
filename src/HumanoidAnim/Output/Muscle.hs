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

    -- * Conversion
  , quaternionToMuscles
  , musclesToQuaternion
  , boneToMuscles
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear (V3(..), Quaternion(..), axisAngle)
import qualified Linear.Quaternion as Q

import HumanoidAnim.Skeleton.Bones (HumanoidBone(..))

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
  LeftLegStretch -> "Left Leg Stretch"
  LeftLegTwistInOut -> "Left Leg Twist In-Out"
  LeftFootUpDown -> "Left Foot Up-Down"
  LeftFootTwistInOut -> "Left Foot Twist In-Out"
  LeftToesUpDown -> "Left Toes Up-Down"

  -- Right Leg
  RightUpperLegFrontBack -> "Right Upper Leg Front-Back"
  RightUpperLegInOut -> "Right Upper Leg In-Out"
  RightUpperLegTwistInOut -> "Right Upper Leg Twist In-Out"
  RightLegStretch -> "Right Leg Stretch"
  RightLegTwistInOut -> "Right Leg Twist In-Out"
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

-- | Convert a quaternion rotation to muscle values for a bone
-- Returns a list of (MuscleId, MuscleValue) pairs
quaternionToMuscles :: HumanoidBone -> Quaternion Float -> [(MuscleId, MuscleValue)]
quaternionToMuscles bone quat =
  let muscles = boneToMuscles bone
      -- Extract euler angles from quaternion
      (pitch, yaw, roll) = quaternionToEuler quat
  in case bone of
    -- Spine bones: Front-Back = pitch, Left-Right = roll, Twist = yaw
    Spine -> zipMuscles muscles [pitch, roll, yaw]
    Chest -> zipMuscles muscles [pitch, roll, yaw]
    UpperChest -> zipMuscles muscles [pitch, roll, yaw]

    -- Neck and Head
    Neck -> zipMuscles muscles [pitch, roll, yaw]
    Head -> zipMuscles muscles [pitch, roll, yaw]
    Jaw -> zipMuscles muscles [pitch, yaw]

    -- Eyes
    LeftEye -> zipMuscles muscles [pitch, yaw]
    RightEye -> zipMuscles muscles [pitch, yaw]

    -- Shoulders: Down-Up, Front-Back
    LeftShoulder -> zipMuscles muscles [roll, pitch]
    RightShoulder -> zipMuscles muscles [roll, pitch]

    -- Upper Arm: Down-Up, Front-Back, Twist
    LeftUpperArm -> zipMuscles muscles [roll, pitch, yaw]
    RightUpperArm -> zipMuscles muscles [roll, pitch, yaw]

    -- Lower Arm: Stretch (bend), Twist
    LeftLowerArm -> zipMuscles muscles [pitch, yaw]
    RightLowerArm -> zipMuscles muscles [pitch, yaw]

    -- Hand: Down-Up, In-Out
    LeftHand -> zipMuscles muscles [pitch, roll]
    RightHand -> zipMuscles muscles [pitch, roll]

    -- Upper Leg: Front-Back, In-Out, Twist
    LeftUpperLeg -> zipMuscles muscles [pitch, roll, yaw]
    RightUpperLeg -> zipMuscles muscles [pitch, roll, yaw]

    -- Lower Leg: Stretch, Twist
    LeftLowerLeg -> zipMuscles muscles [pitch, yaw]
    RightLowerLeg -> zipMuscles muscles [pitch, yaw]

    -- Foot: Up-Down, Twist
    LeftFoot -> zipMuscles muscles [pitch, yaw]
    RightFoot -> zipMuscles muscles [pitch, yaw]

    -- Toes: Up-Down only
    LeftToes -> zipMuscles muscles [pitch]
    RightToes -> zipMuscles muscles [pitch]

    -- Fingers: use simplified mapping
    _ -> zipMuscles muscles (replicate (length muscles) pitch)
  where
    zipMuscles :: [MuscleId] -> [Float] -> [(MuscleId, MuscleValue)]
    zipMuscles mids angles = zipWith toMuscle mids (angles ++ repeat 0)

    toMuscle :: MuscleId -> Float -> (MuscleId, MuscleValue)
    toMuscle mid angle =
      let MuscleRange minA maxA = muscleDefaultRange mid
          -- Convert angle (in degrees) to muscle value (-1 to 1)
          range = maxA - minA
          center = (maxA + minA) / 2
          normalized = if range > 0
                       then clampF (-1) 1 ((angle - center) / (range / 2))
                       else 0
      in (mid, normalized)

-- | Convert muscle values back to quaternion
musclesToQuaternion :: HumanoidBone -> [(MuscleId, MuscleValue)] -> Quaternion Float
musclesToQuaternion bone muscleValues =
  let muscleMap = Map.fromList muscleValues
      muscles = boneToMuscles bone
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

-- | Convert quaternion to Euler angles (pitch, yaw, roll) in degrees
quaternionToEuler :: Quaternion Float -> (Float, Float, Float)
quaternionToEuler (Quaternion w (V3 x y z)) =
  let -- Roll (x-axis rotation)
      sinr_cosp = 2 * (w * x + y * z)
      cosr_cosp = 1 - 2 * (x * x + y * y)
      roll = atan2 sinr_cosp cosr_cosp

      -- Pitch (y-axis rotation)
      sinp = 2 * (w * y - z * x)
      pitch = if abs sinp >= 1
              then signum sinp * (pi / 2)  -- Use 90 degrees if out of range
              else asin sinp

      -- Yaw (z-axis rotation)
      siny_cosp = 2 * (w * z + x * y)
      cosy_cosp = 1 - 2 * (y * y + z * z)
      yaw = atan2 siny_cosp cosy_cosp

  in (toDegrees pitch, toDegrees yaw, toDegrees roll)

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
