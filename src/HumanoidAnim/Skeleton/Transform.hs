-- |
-- Module      : HumanoidAnim.Skeleton.Transform
-- Description : Rotation and transform calculations
--
-- This module provides rotation calculation utilities for bone transforms,
-- including LookAt rotation and pose computation.
module HumanoidAnim.Skeleton.Transform
  ( -- * Rotation Modes
    RotationMode(..)
  , getRotationMode

    -- * Rotation Calculations
  , lookRotation
  , lookRotationWithUp
  , computeRotations
  , computeBoneRotation

    -- * Coordinate Transforms
  , convertZUpToYUp
  , convertYUpToZUp

    -- * Utilities
  , normalizeQuaternion
  , quaternionFromAxisAngle

    -- * Muscle Axis Definitions
  , MuscleAxes(..)
  , tPoseMuscleAxes
  , fetusPoseMuscleAxes
  , rotateVector
  , quaternionToAxisAngles
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Linear
  ( V3(..)
  , Quaternion(..)
  , normalize
  , cross
  , norm
  )

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Hierarchy
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))

-- | Rotation computation mode for a bone
data RotationMode
  = LookAtChild      -- ^ Rotate to face child bone
  | InheritParent    -- ^ Inherit parent's rotation (for terminal bones)
  | FixedRotation (Quaternion Float)  -- ^ Use specified rotation
  deriving stock (Show, Eq)

-- | Determine rotation mode for a bone
getRotationMode
  :: Skeleton
  -> HumanoidBone
  -> Maybe (Quaternion Float)  -- ^ Optional fixed rotation
  -> RotationMode
getRotationMode skel bone maybeRot = case maybeRot of
  Just rot -> FixedRotation rot
  Nothing
    | isTerminalBone skel bone -> InheritParent
    | otherwise -> LookAtChild

-- | Check if a bone is terminal (has no children in the active skeleton)
isTerminalBone :: Skeleton -> HumanoidBone -> Bool
isTerminalBone skel bone =
  null [b | b <- Set.toList (skeletonActiveBones skel), boneParent b == Just bone]

-- | Calculate look rotation (rotation that points forward vector at target)
-- Assumes Y-up coordinate system
lookRotation :: V3 Float -> Quaternion Float
lookRotation forward = lookRotationWithUp forward (V3 0 1 0)

-- | Calculate look rotation with custom up vector
-- Creates a local coordinate system where:
--   X-axis = forward direction (child direction, used for Twist)
--   Z-axis = up direction (used for Down-Up rotation axis)
--   Y-axis = computed from cross product (used for Front-Back rotation axis)
lookRotationWithUp :: V3 Float -> V3 Float -> Quaternion Float
lookRotationWithUp forward up
  | norm forward < 0.0001 = Quaternion 1 (V3 0 0 0)  -- Identity
  | otherwise = matrixToQuaternion xAxis yAxis zAxis
  where
    xAxis = normalize forward  -- X = child direction (Twist axis)
    -- Handle case where forward is parallel to up
    rawZ = if norm (cross up xAxis) < 0.0001
           then let altUp = if abs (xAxis `dot` V3 0 0 1) < 0.9
                            then V3 0 0 1
                            else V3 1 0 0
                in normalize $ cross xAxis altUp
           else normalize $ cross xAxis up
    zAxis = rawZ  -- Z = up direction (Down-Up axis)
    yAxis = cross zAxis xAxis  -- Y = right direction (Front-Back axis)
    dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | Convert rotation matrix (as column vectors) to quaternion
matrixToQuaternion :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float
matrixToQuaternion (V3 m00 m10 m20) (V3 m01 m11 m21) (V3 m02 m12 m22) =
  normalizeQuaternion $ case () of
    _ | trace > 0 ->
          let s = 0.5 / sqrt (trace + 1)
          in Quaternion (0.25 / s) (V3 ((m21 - m12) * s) ((m02 - m20) * s) ((m10 - m01) * s))
      | m00 > m11 && m00 > m22 ->
          let s = 2 * sqrt (1 + m00 - m11 - m22)
          in Quaternion ((m21 - m12) / s) (V3 (0.25 * s) ((m01 + m10) / s) ((m02 + m20) / s))
      | m11 > m22 ->
          let s = 2 * sqrt (1 + m11 - m00 - m22)
          in Quaternion ((m02 - m20) / s) (V3 ((m01 + m10) / s) (0.25 * s) ((m12 + m21) / s))
      | otherwise ->
          let s = 2 * sqrt (1 + m22 - m00 - m11)
          in Quaternion ((m10 - m01) / s) (V3 ((m02 + m20) / s) ((m12 + m21) / s) (0.25 * s))
  where
    trace = m00 + m11 + m22

-- | Normalize a quaternion
normalizeQuaternion :: Quaternion Float -> Quaternion Float
normalizeQuaternion (Quaternion w (V3 x y z)) =
  let len = sqrt (w*w + x*x + y*y + z*z)
  in if len < 0.0001
     then Quaternion 1 (V3 0 0 0)
     else Quaternion (w/len) (V3 (x/len) (y/len) (z/len))

-- | Create quaternion from axis and angle (radians)
quaternionFromAxisAngle :: V3 Float -> Float -> Quaternion Float
quaternionFromAxisAngle axis angle =
  let halfAngle = angle / 2
      s = sin halfAngle
      V3 x y z = normalize axis
  in Quaternion (cos halfAngle) (V3 (x * s) (y * s) (z * s))

-- | Compute rotations for all bones in a pose
computeRotations
  :: Skeleton
  -> Map HumanoidBone (V3 Float)  -- ^ Bone positions
  -> Map HumanoidBone (Maybe (Quaternion Float))  -- ^ Fixed rotations
  -> Map HumanoidBone Transform
computeRotations skel positions fixedRots =
  Map.mapWithKey computeTransform positions
  where
    computeTransform bone pos = Transform
      { transformPosition = pos
      , transformRotation = computeBoneRotation skel positions fixedRots bone
      }

-- | Compute rotation for a single bone
computeBoneRotation
  :: Skeleton
  -> Map HumanoidBone (V3 Float)  -- ^ Bone positions
  -> Map HumanoidBone (Maybe (Quaternion Float))  -- ^ Fixed rotations
  -> HumanoidBone
  -> Quaternion Float
computeBoneRotation skel positions fixedRots bone =
  case getRotationMode skel bone (join $ Map.lookup bone fixedRots) of
    FixedRotation rot -> rot
    InheritParent -> case boneParent bone of
      Just parent -> computeBoneRotation skel positions fixedRots parent
      Nothing -> Quaternion 1 (V3 0 0 0)  -- Identity for root
    LookAtChild ->
      case getFirstActiveChild skel bone of
        Just child ->
          case (Map.lookup bone positions, Map.lookup child positions) of
            (Just bonePos, Just childPos) ->
              let forward = childPos - bonePos
                  up = boneUpVector bone
              in lookRotationWithUp forward up
            _ -> Quaternion 1 (V3 0 0 0)
        Nothing -> Quaternion 1 (V3 0 0 0)
  where
    join Nothing = Nothing
    join (Just x) = x

-- | Get the up vector for a bone's local coordinate system
-- This determines how the X and Y axes are oriented when Z points toward the child
boneUpVector :: HumanoidBone -> V3 Float
boneUpVector bone = case bone of
  -- Arm bones: use forward direction (+Z) as up reference
  -- This aligns Y-axis with front-back direction for Front-Back muscle
  RightShoulder -> V3 0 0 1
  LeftShoulder  -> V3 0 0 1
  RightUpperArm -> V3 0 0 1
  LeftUpperArm  -> V3 0 0 1
  RightLowerArm -> V3 0 0 1
  LeftLowerArm  -> V3 0 0 1
  -- Default: use global up (+Y)
  _ -> V3 0 1 0

-- | Get the first active child of a bone
getFirstActiveChild :: Skeleton -> HumanoidBone -> Maybe HumanoidBone
getFirstActiveChild skel bone =
  case [b | b <- Set.toList (skeletonActiveBones skel), boneParent b == Just bone] of
    (child:_) -> Just child
    [] -> Nothing

-- | Convert Z-up coordinate (Blender) to Y-up (Unity/glTF)
convertZUpToYUp :: V3 Float -> V3 Float
convertZUpToYUp (V3 x y z) = V3 x z (-y)

-- | Convert Y-up coordinate to Z-up (Blender)
convertYUpToZUp :: V3 Float -> V3 Float
convertYUpToZUp (V3 x y z) = V3 x (-z) y

-- ============================================================================
-- Muscle Axis Definitions
-- ============================================================================

-- | Local coordinate axes for muscle calculation
-- Based on Unity Humanoid specification:
--   X (DoF 0) = Twist axis (bone elongation direction)
--   Y (DoF 1) = Spread axis (in-out / left-right)
--   Z (DoF 2) = Stretch axis (front-back / down-up)
data MuscleAxes = MuscleAxes
  { axisX :: V3 Float  -- ^ Twist axis
  , axisY :: V3 Float  -- ^ Spread axis
  , axisZ :: V3 Float  -- ^ Stretch axis
  } deriving stock (Show, Eq)

-- | T-Pose muscle axes in world coordinates
-- From docs/muscle_axes_table.md
tPoseMuscleAxes :: HumanoidBone -> MuscleAxes
tPoseMuscleAxes bone = case bone of
  -- Torso: Twist=+Y, Spread=+X, Stretch=+Z
  Hips       -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)
  Spine      -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)
  Chest      -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)
  UpperChest -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)
  Neck       -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)
  Head       -> MuscleAxes (V3 0 1 0) (V3 1 0 0) (V3 0 0 1)

  -- Left Leg: Twist=-Y, Spread=-X, Stretch=+Z
  LeftUpperLeg -> MuscleAxes (V3 0 (-1) 0) (V3 (-1) 0 0) (V3 0 0 1)
  LeftLowerLeg -> MuscleAxes (V3 0 (-1) 0) (V3 (-1) 0 0) (V3 0 0 1)
  LeftFoot     -> MuscleAxes (V3 0 0 1) (V3 (-1) 0 0) (V3 0 1 0)

  -- Right Leg: Twist=-Y, Spread=-X, Stretch=+Z (same as left in T-pose)
  RightUpperLeg -> MuscleAxes (V3 0 (-1) 0) (V3 (-1) 0 0) (V3 0 0 1)
  RightLowerLeg -> MuscleAxes (V3 0 (-1) 0) (V3 (-1) 0 0) (V3 0 0 1)
  RightFoot     -> MuscleAxes (V3 0 0 1) (V3 (-1) 0 0) (V3 0 1 0)

  -- Left Arm: Twist=-X, Spread=+Y, Stretch=+Z
  LeftShoulder -> MuscleAxes (V3 (-1) 0 0) (V3 0 1 0) (V3 0 0 1)
  LeftUpperArm -> MuscleAxes (V3 (-1) 0 0) (V3 0 1 0) (V3 0 0 1)
  LeftLowerArm -> MuscleAxes (V3 (-1) 0 0) (V3 0 1 0) (V3 0 0 1)
  LeftHand     -> MuscleAxes (V3 (-1) 0 0) (V3 0 1 0) (V3 0 0 1)

  -- Right Arm: Twist=+X, Spread=-Y, Stretch=+Z
  RightShoulder -> MuscleAxes (V3 1 0 0) (V3 0 (-1) 0) (V3 0 0 1)
  RightUpperArm -> MuscleAxes (V3 1 0 0) (V3 0 (-1) 0) (V3 0 0 1)
  RightLowerArm -> MuscleAxes (V3 1 0 0) (V3 0 (-1) 0) (V3 0 0 1)
  RightHand     -> MuscleAxes (V3 1 0 0) (V3 0 (-1) 0) (V3 0 0 1)

  -- Default: identity axes
  _ -> MuscleAxes (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- | T-Pose to Fetus Pose rotation for each bone
-- These rotations transform from T-Pose to Fetus Pose
tPoseToFetusRotation :: HumanoidBone -> Quaternion Float
tPoseToFetusRotation bone = case bone of
  -- Upper Arm: T-pose (horizontal) -> Fetus (hanging down)
  -- Arm Down-Up T-Pose = 0.4, center = -40°
  -- Angle from T-Pose to Fetus = 40° rotation around Z axis
  RightUpperArm -> quaternionFromAxisAngle (V3 0 0 1) ((-40) * pi / 180)
  LeftUpperArm  -> quaternionFromAxisAngle (V3 0 0 1) (40 * pi / 180)

  -- Lower Arm: T-pose (extended) -> Fetus (bent ~90°)
  -- Forearm Stretch T-Pose = 1.0, center = -80°
  -- Fetus is bent forward
  RightLowerArm -> quaternionFromAxisAngle (V3 0 0 1) (80 * pi / 180)
  LeftLowerArm  -> quaternionFromAxisAngle (V3 0 0 1) ((-80) * pi / 180)

  -- Upper Leg: T-pose (straight down) -> Fetus (raised forward)
  -- Leg Front-Back T-Pose = 0.6, center = -30°
  -- Fetus has legs forward by ~30°
  RightUpperLeg -> quaternionFromAxisAngle (V3 0 0 1) (30 * pi / 180)
  LeftUpperLeg  -> quaternionFromAxisAngle (V3 0 0 1) (30 * pi / 180)

  -- Lower Leg: T-pose (extended) -> Fetus (bent ~90°)
  -- Leg Stretch T-Pose = 1.0, center = -80°
  RightLowerLeg -> quaternionFromAxisAngle (V3 0 0 1) (80 * pi / 180)
  LeftLowerLeg  -> quaternionFromAxisAngle (V3 0 0 1) (80 * pi / 180)

  -- Other bones: no rotation difference
  _ -> Quaternion 1 (V3 0 0 0)

-- | Rotate a vector by a quaternion
-- q * v * q^(-1)
rotateVector :: Quaternion Float -> V3 Float -> V3 Float
rotateVector (Quaternion w (V3 qx qy qz)) (V3 vx vy vz) =
  let -- Quaternion multiplication: q * (0, v) * q^(-1)
      -- Using the formula: v' = v + 2*w*(q_xyz × v) + 2*(q_xyz × (q_xyz × v))
      qv = V3 qx qy qz
      uv = cross qv (V3 vx vy vz)
      uuv = cross qv uv
  in V3 vx vy vz + fmap (* (2 * w)) uv + fmap (* 2) uuv

-- | Fetus Pose muscle axes in world coordinates
-- Computed by rotating T-Pose axes by the T-Pose to Fetus rotation
fetusPoseMuscleAxes :: HumanoidBone -> MuscleAxes
fetusPoseMuscleAxes bone =
  let tAxes = tPoseMuscleAxes bone
      rot = tPoseToFetusRotation bone
  in MuscleAxes
       { axisX = rotateVector rot (axisX tAxes)
       , axisY = rotateVector rot (axisY tAxes)
       , axisZ = rotateVector rot (axisZ tAxes)
       }

-- | Extract rotation angles around custom axes from a quaternion
-- Given a quaternion representing a rotation and custom X, Y, Z axes,
-- decompose the rotation into angles around each axis.
--
-- This uses swing-twist decomposition approach:
-- 1. Project the quaternion rotation onto each axis
-- 2. Extract the angle of rotation around that axis
--
-- Returns (angleX, angleY, angleZ) in degrees
quaternionToAxisAngles :: MuscleAxes -> Quaternion Float -> (Float, Float, Float)
quaternionToAxisAngles axes quat =
  let -- Extract angle around each axis using swing-twist decomposition
      angleX = extractAngleAroundAxis (axisX axes) quat
      angleY = extractAngleAroundAxis (axisY axes) quat
      angleZ = extractAngleAroundAxis (axisZ axes) quat
  in (angleX, angleY, angleZ)

-- | Extract the rotation angle around a specific axis from a quaternion
-- Uses the swing-twist decomposition method
extractAngleAroundAxis :: V3 Float -> Quaternion Float -> Float
extractAngleAroundAxis axis (Quaternion w (V3 qx qy qz)) =
  let -- Project quaternion's vector part onto the axis
      V3 ax ay az = normalize axis
      -- Dot product: projection of qv onto axis
      projection = qx * ax + qy * ay + qz * az
      -- Twist quaternion: rotation around the axis
      -- The twist quaternion is (w, projection * axis), normalized
      -- Normalize the twist quaternion
      twistLen = sqrt (w * w + projection * projection)
      normW = if twistLen < 0.0001 then 1 else w / twistLen
      normProj = if twistLen < 0.0001 then 0 else projection / twistLen
      -- Extract angle: angle = 2 * atan2(|twist.xyz|, twist.w)
      -- Since twist.xyz is aligned with axis, |twist.xyz| = |normProj|
      angle = 2 * atan2 (abs normProj) normW
      -- Determine sign based on projection direction
      signedAngle = if normProj >= 0 then angle else (-angle)
  in signedAngle * 180 / pi  -- Convert to degrees
