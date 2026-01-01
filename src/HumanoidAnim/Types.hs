{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : HumanoidAnim.Types
-- Description : Common type definitions for humanoid animation
--
-- This module defines the core types used throughout the humanoid animation
-- generator, including positions, rotations, transforms, and poses.
module HumanoidAnim.Types
  ( -- * Basic Types
    Position
  , Rotation
  , Transform(..)
  , identityTransform

    -- * Pose Types
  , Pose
  , FullPose

    -- * Motion Types
  , Trajectory
  , Time
  , FrameNumber

    -- * Re-exports from Linear
  , V3(..)
  , Quaternion(..)
  ) where

import Data.Map.Strict (Map)
import Linear (Quaternion (..), V3 (..))

import HumanoidAnim.Skeleton.Bones (HumanoidBone)

-- | 3D position in meters
type Position = V3 Float

-- | Rotation as quaternion (x, y, z, w)
type Rotation = Quaternion Float

-- | Transform containing position and rotation
data Transform = Transform
  { transformPosition :: Position
  , transformRotation :: Rotation
  } deriving stock (Show, Eq)

-- | Identity transform (origin position, no rotation)
identityTransform :: Transform
identityTransform = Transform
  { transformPosition = V3 0 0 0
  , transformRotation = Quaternion 1 (V3 0 0 0)  -- Identity quaternion (w=1, xyz=0)
  }

-- | Pose mapping bones to positions
type Pose = Map HumanoidBone Position

-- | Full pose mapping bones to transforms (position + rotation)
type FullPose = Map HumanoidBone Transform

-- | Trajectory function: normalized time (0-1) -> position
type Trajectory = Float -> Position

-- | Time in seconds
type Time = Float

-- | Frame number (0-indexed)
type FrameNumber = Int
