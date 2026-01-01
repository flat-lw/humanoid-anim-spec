-- |
-- Module      : HumanoidAnim.IK.Constraints
-- Description : Joint angle constraints for IK
--
-- This module defines angle constraints for humanoid joints,
-- particularly for elbows and knees which have limited range of motion.
module HumanoidAnim.IK.Constraints
  ( -- * Constraint Types
    JointConstraint(..)
  , ConstraintAxis(..)
  , AngleRange(..)

    -- * Default Constraints
  , defaultJointConstraints
  , getJointConstraint

    -- * Constraint Application
  , applyConstraints
  , clampAngle
  , constrainRotation
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear (V3(..), Quaternion(..), normalize, cross, dot, (^-^), (*^))
import qualified Linear.Quaternion as Q

import HumanoidAnim.Skeleton.Bones

-- | Axis for rotation constraint
data ConstraintAxis
  = AxisX  -- ^ Rotation around X axis (pitch, e.g., elbow/knee bend)
  | AxisY  -- ^ Rotation around Y axis (yaw, e.g., twist)
  | AxisZ  -- ^ Rotation around Z axis (roll)
  deriving stock (Show, Eq, Ord)

-- | Angle range in radians
data AngleRange = AngleRange
  { angleMin :: Float  -- ^ Minimum angle (radians)
  , angleMax :: Float  -- ^ Maximum angle (radians)
  } deriving stock (Show, Eq)

-- | Joint constraint definition
data JointConstraint = JointConstraint
  { constraintBone :: HumanoidBone
  , constraintAxes :: Map ConstraintAxis AngleRange
  } deriving stock (Show, Eq)

-- | Default joint constraints for humanoid skeleton
-- Based on typical human joint limits
defaultJointConstraints :: Map HumanoidBone JointConstraint
defaultJointConstraints = Map.fromList
  [ -- Elbow constraints (hinge joint, bends backward only)
    (LeftLowerArm, JointConstraint LeftLowerArm $ Map.fromList
      [ (AxisX, AngleRange 0 (pi * 0.85))  -- 0 to ~150 degrees flexion
      , (AxisY, AngleRange (-pi/12) (pi/12))  -- Limited twist
      , (AxisZ, AngleRange (-pi/6) (pi/6))  -- Limited roll
      ])
  , (RightLowerArm, JointConstraint RightLowerArm $ Map.fromList
      [ (AxisX, AngleRange 0 (pi * 0.85))  -- 0 to ~150 degrees flexion
      , (AxisY, AngleRange (-pi/12) (pi/12))  -- Limited twist
      , (AxisZ, AngleRange (-pi/6) (pi/6))  -- Limited roll
      ])

    -- Knee constraints (hinge joint, bends backward only)
  , (LeftLowerLeg, JointConstraint LeftLowerLeg $ Map.fromList
      [ (AxisX, AngleRange 0 (pi * 0.8))  -- 0 to ~145 degrees flexion
      , (AxisY, AngleRange (-pi/36) (pi/36))  -- Very limited twist
      , (AxisZ, AngleRange (-pi/36) (pi/36))  -- Very limited roll
      ])
  , (RightLowerLeg, JointConstraint RightLowerLeg $ Map.fromList
      [ (AxisX, AngleRange 0 (pi * 0.8))  -- 0 to ~145 degrees flexion
      , (AxisY, AngleRange (-pi/36) (pi/36))  -- Very limited twist
      , (AxisZ, AngleRange (-pi/36) (pi/36))  -- Very limited roll
      ])

    -- Shoulder constraints (ball and socket, wide range)
  , (LeftUpperArm, JointConstraint LeftUpperArm $ Map.fromList
      [ (AxisX, AngleRange (-pi * 0.5) (pi * 0.5))  -- -90 to 90 degrees
      , (AxisY, AngleRange (-pi * 0.5) (pi * 0.5))
      , (AxisZ, AngleRange (-pi * 0.75) (pi * 0.25))  -- More limited backward
      ])
  , (RightUpperArm, JointConstraint RightUpperArm $ Map.fromList
      [ (AxisX, AngleRange (-pi * 0.5) (pi * 0.5))
      , (AxisY, AngleRange (-pi * 0.5) (pi * 0.5))
      , (AxisZ, AngleRange (-pi * 0.25) (pi * 0.75))
      ])

    -- Hip constraints (ball and socket)
  , (LeftUpperLeg, JointConstraint LeftUpperLeg $ Map.fromList
      [ (AxisX, AngleRange (-pi * 0.65) (pi * 0.35))  -- Forward/backward
      , (AxisY, AngleRange (-pi * 0.25) (pi * 0.25))  -- Twist
      , (AxisZ, AngleRange (-pi * 0.25) (pi * 0.5))   -- Side spread
      ])
  , (RightUpperLeg, JointConstraint RightUpperLeg $ Map.fromList
      [ (AxisX, AngleRange (-pi * 0.65) (pi * 0.35))
      , (AxisY, AngleRange (-pi * 0.25) (pi * 0.25))
      , (AxisZ, AngleRange (-pi * 0.5) (pi * 0.25))
      ])

    -- Spine constraints (limited)
  , (Spine, JointConstraint Spine $ Map.fromList
      [ (AxisX, AngleRange (-pi/6) (pi/4))  -- Bend forward/backward
      , (AxisY, AngleRange (-pi/6) (pi/6))  -- Twist
      , (AxisZ, AngleRange (-pi/8) (pi/8))  -- Side bend
      ])
  , (Chest, JointConstraint Chest $ Map.fromList
      [ (AxisX, AngleRange (-pi/8) (pi/6))
      , (AxisY, AngleRange (-pi/6) (pi/6))
      , (AxisZ, AngleRange (-pi/8) (pi/8))
      ])

    -- Neck constraints
  , (Neck, JointConstraint Neck $ Map.fromList
      [ (AxisX, AngleRange (-pi/4) (pi/3))  -- Nod
      , (AxisY, AngleRange (-pi/3) (pi/3))  -- Turn
      , (AxisZ, AngleRange (-pi/6) (pi/6))  -- Tilt
      ])
  ]

-- | Get constraint for a specific bone
getJointConstraint :: HumanoidBone -> Maybe JointConstraint
getJointConstraint bone = Map.lookup bone defaultJointConstraints

-- | Apply constraints to a rotation quaternion for a specific bone
applyConstraints :: HumanoidBone -> Quaternion Float -> Quaternion Float
applyConstraints bone quat =
  case getJointConstraint bone of
    Nothing -> quat  -- No constraints for this bone
    Just constraint -> constrainRotation constraint quat

-- | Constrain a rotation based on joint constraints
constrainRotation :: JointConstraint -> Quaternion Float -> Quaternion Float
constrainRotation constraint quat =
  let -- Extract Euler angles (approximate)
      (rx, ry, rz) = quatToEuler quat

      -- Apply constraints per axis
      axes = constraintAxes constraint
      rx' = case Map.lookup AxisX axes of
              Just range -> clampAngle range rx
              Nothing -> rx
      ry' = case Map.lookup AxisY axes of
              Just range -> clampAngle range ry
              Nothing -> ry
      rz' = case Map.lookup AxisZ axes of
              Just range -> clampAngle range rz
              Nothing -> rz

  in eulerToQuat rx' ry' rz'

-- | Clamp an angle to a range
clampAngle :: AngleRange -> Float -> Float
clampAngle (AngleRange minA maxA) angle =
  max minA (min maxA angle)

-- | Convert quaternion to Euler angles (XYZ order)
-- Note: This is an approximation and may have gimbal lock issues
quatToEuler :: Quaternion Float -> (Float, Float, Float)
quatToEuler (Quaternion w (V3 x y z)) =
  let -- Roll (X-axis rotation)
      sinr_cosp = 2 * (w * x + y * z)
      cosr_cosp = 1 - 2 * (x * x + y * y)
      roll = atan2 sinr_cosp cosr_cosp

      -- Pitch (Y-axis rotation)
      sinp = 2 * (w * y - z * x)
      pitch = if abs sinp >= 1
              then signum sinp * (pi / 2)  -- Use 90 degrees if out of range
              else asin sinp

      -- Yaw (Z-axis rotation)
      siny_cosp = 2 * (w * z + x * y)
      cosy_cosp = 1 - 2 * (y * y + z * z)
      yaw = atan2 siny_cosp cosy_cosp

  in (roll, pitch, yaw)

-- | Convert Euler angles to quaternion (XYZ order)
eulerToQuat :: Float -> Float -> Float -> Quaternion Float
eulerToQuat roll pitch yaw =
  let cr = cos (roll / 2)
      sr = sin (roll / 2)
      cp = cos (pitch / 2)
      sp = sin (pitch / 2)
      cy = cos (yaw / 2)
      sy = sin (yaw / 2)

      w = cr * cp * cy + sr * sp * sy
      x = sr * cp * cy - cr * sp * sy
      y = cr * sp * cy + sr * cp * sy
      z = cr * cp * sy - sr * sp * cy

  in Quaternion w (V3 x y z)
