-- |
-- Module      : HumanoidAnim.Motion.Keyframe
-- Description : Keyframe data types and utilities
--
-- This module defines keyframe structures for animation and provides
-- utilities for working with keyframe sequences.
module HumanoidAnim.Motion.Keyframe
  ( -- * Keyframe Type
    Keyframe(..)

    -- * Keyframe Construction
  , keyframe
  , keyframeWithRotation

    -- * Keyframe Queries
  , keyframeDuration
  , keyframeCount
  , sortKeyframes
  , validateKeyframes
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Motion.Easing (Easing(..))

-- | A keyframe in an animation
data Keyframe = Keyframe
  { keyTime :: Float
    -- ^ Time in seconds
  , keyPosition :: V3 Float
    -- ^ Target position in meters
  , keyRotation :: Maybe (Quaternion Float)
    -- ^ Optional target rotation (if Nothing, computed from motion)
  , keyEasing :: Easing
    -- ^ Easing function for interpolation TO this keyframe
  } deriving stock (Show)

instance Eq Keyframe where
  k1 == k2 = keyTime k1 == keyTime k2
          && keyPosition k1 == keyPosition k2
          && keyRotation k1 == keyRotation k2
          -- Note: easing comparison may fail for Custom functions

-- | Create a simple keyframe (linear easing, no rotation)
keyframe :: Float -> V3 Float -> Keyframe
keyframe time pos = Keyframe
  { keyTime = time
  , keyPosition = pos
  , keyRotation = Nothing
  , keyEasing = Linear
  }

-- | Create a keyframe with rotation
keyframeWithRotation :: Float -> V3 Float -> Quaternion Float -> Keyframe
keyframeWithRotation time pos rot = Keyframe
  { keyTime = time
  , keyPosition = pos
  , keyRotation = Just rot
  , keyEasing = Linear
  }

-- | Get the total duration from a list of keyframes
keyframeDuration :: [Keyframe] -> Float
keyframeDuration [] = 0
keyframeDuration kfs = maximum (map keyTime kfs) - minimum (map keyTime kfs)

-- | Get the number of keyframes
keyframeCount :: [Keyframe] -> Int
keyframeCount = length

-- | Sort keyframes by time
sortKeyframes :: [Keyframe] -> [Keyframe]
sortKeyframes = sortBy (comparing keyTime)

-- | Validate keyframes, returning errors if any
validateKeyframes :: [Keyframe] -> [String]
validateKeyframes kfs = concat
  [ checkMinimumCount
  , checkTimeOrder
  , checkDuplicateTimes
  ]
  where
    sorted = sortKeyframes kfs

    checkMinimumCount
      | length kfs < 2 = ["At least 2 keyframes required, got " ++ show (length kfs)]
      | otherwise = []

    checkTimeOrder =
      [ "Keyframe times must be non-negative, got " ++ show t
      | kf <- kfs
      , let t = keyTime kf
      , t < 0
      ]

    checkDuplicateTimes =
      [ "Duplicate keyframe time: " ++ show t
      | (k1, k2) <- zip sorted (tail sorted)
      , let t = keyTime k1
      , keyTime k1 == keyTime k2
      ]
