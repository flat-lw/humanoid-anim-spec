-- |
-- Module      : HumanoidAnim.Motion.Trajectory
-- Description : Trajectory generation from keyframes
--
-- This module provides functions to create trajectory functions from
-- keyframe sequences for use in animation generation.
module HumanoidAnim.Motion.Trajectory
  ( -- * Trajectory Type
    Trajectory

    -- * Trajectory Construction
  , keyframesToTrajectory
  , linearTrajectory
  , bezierTrajectory

    -- * Trajectory Sampling
  , sampleTrajectory
  , sampleTrajectoryUniform
  ) where

import Linear (V3(..), (^*), (^+^), (^-^))

import HumanoidAnim.Motion.Keyframe (Keyframe(..), sortKeyframes, keyframeDuration)
import HumanoidAnim.Motion.Interpolation (interpolateKeyframes, clamp)

-- | Trajectory function type: time (seconds) -> position
type Trajectory = Float -> V3 Float

-- | Create a trajectory from keyframes
-- Takes absolute time in seconds and returns interpolated position
keyframesToTrajectory :: [Keyframe] -> Trajectory
keyframesToTrajectory kfs time = interpolateKeyframes sortedKfs clampedTime
  where
    sortedKfs = sortKeyframes kfs
    minTime = case sortedKfs of
      (k:_) -> keyTime k
      [] -> 0
    maxTime = case sortedKfs of
      [] -> 0
      _ -> keyTime (last sortedKfs)
    clampedTime = clamp minTime maxTime time

-- | Create a simple linear trajectory between two points
linearTrajectory :: V3 Float -> V3 Float -> Float -> Trajectory
linearTrajectory start end duration t =
  let normalizedT = clamp 0 1 (t / duration)
  in start ^+^ ((end ^-^ start) ^* normalizedT)

-- | Create a cubic Bezier trajectory
-- Takes start, control1, control2, end points and duration
bezierTrajectory
  :: V3 Float  -- ^ Start point
  -> V3 Float  -- ^ Control point 1
  -> V3 Float  -- ^ Control point 2
  -> V3 Float  -- ^ End point
  -> Float     -- ^ Duration in seconds
  -> Trajectory
bezierTrajectory p0 p1 p2 p3 duration t =
  let u = clamp 0 1 (t / duration)
      u2 = u * u
      u3 = u2 * u
      oneMinusU = 1 - u
      oneMinusU2 = oneMinusU * oneMinusU
      oneMinusU3 = oneMinusU2 * oneMinusU
  in (p0 ^* oneMinusU3)
     ^+^ (p1 ^* (3 * oneMinusU2 * u))
     ^+^ (p2 ^* (3 * oneMinusU * u2))
     ^+^ (p3 ^* u3)

-- | Sample a trajectory at specific times
sampleTrajectory :: Trajectory -> [Float] -> [V3 Float]
sampleTrajectory traj times = map traj times

-- | Sample a trajectory uniformly at given frame rate
sampleTrajectoryUniform
  :: Trajectory
  -> Float     -- ^ Start time
  -> Float     -- ^ End time
  -> Float     -- ^ Frame rate (fps)
  -> [(Float, V3 Float)]  -- ^ List of (time, position) pairs
sampleTrajectoryUniform traj startTime endTime fps =
  let frameTime = 1 / fps
      times = takeWhile (<= endTime) [startTime, startTime + frameTime ..]
  in [(t, traj t) | t <- times]
