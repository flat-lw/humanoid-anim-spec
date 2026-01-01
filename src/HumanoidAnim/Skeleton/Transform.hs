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
lookRotationWithUp :: V3 Float -> V3 Float -> Quaternion Float
lookRotationWithUp forward up
  | norm forward < 0.0001 = Quaternion 1 (V3 0 0 0)  -- Identity
  | otherwise = matrixToQuaternion right up' forward'
  where
    forward' = normalize forward
    right = normalize $ cross up forward'
    up' = cross forward' right

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
normalizeQuaternion q@(Quaternion w (V3 x y z)) =
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
              lookRotation (childPos - bonePos)
            _ -> Quaternion 1 (V3 0 0 0)
        Nothing -> Quaternion 1 (V3 0 0 0)
  where
    join Nothing = Nothing
    join (Just x) = x

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
