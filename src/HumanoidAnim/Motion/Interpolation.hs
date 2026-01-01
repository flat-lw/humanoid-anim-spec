-- |
-- Module      : HumanoidAnim.Motion.Interpolation
-- Description : Interpolation utilities for animation
--
-- This module provides interpolation functions for positions, rotations,
-- and full transforms used in animation blending.
module HumanoidAnim.Motion.Interpolation
  ( -- * Position Interpolation
    lerpPosition
  , lerpV3

    -- * Rotation Interpolation
  , slerpQuaternion
  , nlerpQuaternion

    -- * Transform Interpolation
  , lerpTransform

    -- * Keyframe Interpolation
  , interpolateKeyframes
  , findKeyframePair

    -- * Utilities
  , clamp
  , remap
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Linear
  ( V3(..)
  , Quaternion(..)
  , dot
  , normalize
  , (^*)
  , (^+^)
  , (^-^)
  )

import HumanoidAnim.Motion.Easing (Easing(..), applyEasing)
import HumanoidAnim.Motion.Keyframe (Keyframe(..), sortKeyframes)

-- | Linear interpolation for V3
lerpV3 :: V3 Float -> V3 Float -> Float -> V3 Float
lerpV3 a b t = a ^+^ ((b ^-^ a) ^* t)

-- | Linear interpolation for positions (alias)
lerpPosition :: V3 Float -> V3 Float -> Float -> V3 Float
lerpPosition = lerpV3

-- | Spherical linear interpolation for quaternions
slerpQuaternion :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float
slerpQuaternion q1 q2' t
  | abs cosHalfTheta >= 1.0 = q1  -- Quaternions are the same
  | abs sinHalfTheta < 0.001 =    -- Linear interpolation for small angles
      normalizeQuat $ Quaternion
        (w1 * 0.5 + w2 * 0.5)
        (V3 (x1 * 0.5 + x2 * 0.5)
            (y1 * 0.5 + y2 * 0.5)
            (z1 * 0.5 + z2 * 0.5))
  | otherwise =
      let ratioA = sin ((1 - t) * halfTheta) / sinHalfTheta
          ratioB = sin (t * halfTheta) / sinHalfTheta
      in normalizeQuat $ Quaternion
           (w1 * ratioA + w2 * ratioB)
           (V3 (x1 * ratioA + x2 * ratioB)
               (y1 * ratioA + y2 * ratioB)
               (z1 * ratioA + z2 * ratioB))
  where
    -- Ensure shortest path
    Quaternion w1 (V3 x1 y1 z1) = q1
    dotProduct = quatDot q1 q2'
    q2 = if dotProduct < 0 then negateQuat q2' else q2'
    Quaternion w2 (V3 x2 y2 z2) = q2

    cosHalfTheta = abs dotProduct
    halfTheta = acos cosHalfTheta
    sinHalfTheta = sqrt (1 - cosHalfTheta * cosHalfTheta)

-- | Normalized linear interpolation for quaternions (faster but less accurate)
nlerpQuaternion :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float
nlerpQuaternion q1 q2' t = normalizeQuat $ Quaternion
  (w1 * (1 - t) + w2 * t)
  (V3 (x1 * (1 - t) + x2 * t)
      (y1 * (1 - t) + y2 * t)
      (z1 * (1 - t) + z2 * t))
  where
    Quaternion w1 (V3 x1 y1 z1) = q1
    q2 = if quatDot q1 q2' < 0 then negateQuat q2' else q2'
    Quaternion w2 (V3 x2 y2 z2) = q2

-- | Quaternion dot product
quatDot :: Quaternion Float -> Quaternion Float -> Float
quatDot (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2

-- | Negate quaternion
negateQuat :: Quaternion Float -> Quaternion Float
negateQuat (Quaternion w (V3 x y z)) = Quaternion (-w) (V3 (-x) (-y) (-z))

-- | Normalize quaternion
normalizeQuat :: Quaternion Float -> Quaternion Float
normalizeQuat q@(Quaternion w (V3 x y z)) =
  let len = sqrt (w*w + x*x + y*y + z*z)
  in if len < 0.0001
     then Quaternion 1 (V3 0 0 0)
     else Quaternion (w/len) (V3 (x/len) (y/len) (z/len))

-- | Transform type for interpolation
data Transform = Transform
  { transformPosition :: V3 Float
  , transformRotation :: Quaternion Float
  } deriving stock (Show, Eq)

-- | Interpolate between two transforms
lerpTransform :: Transform -> Transform -> Float -> Transform
lerpTransform t1 t2 alpha = Transform
  { transformPosition = lerpPosition (transformPosition t1) (transformPosition t2) alpha
  , transformRotation = slerpQuaternion (transformRotation t1) (transformRotation t2) alpha
  }

-- | Interpolate position from keyframes at given time
interpolateKeyframes :: [Keyframe] -> Float -> V3 Float
interpolateKeyframes [] _ = V3 0 0 0
interpolateKeyframes [kf] _ = keyPosition kf
interpolateKeyframes kfs time =
  case findKeyframePair sortedKfs time of
    (Just k1, Just k2) ->
      let t1 = keyTime k1
          t2 = keyTime k2
          localT = if t2 == t1 then 0 else (time - t1) / (t2 - t1)
          easedT = applyEasing (keyEasing k2) localT
      in lerpPosition (keyPosition k1) (keyPosition k2) easedT
    (Just k1, Nothing) -> keyPosition k1
    (Nothing, Just k2) -> keyPosition k2
    (Nothing, Nothing) -> V3 0 0 0
  where
    sortedKfs = sortKeyframes kfs

-- | Find the pair of keyframes surrounding a given time
findKeyframePair :: [Keyframe] -> Float -> (Maybe Keyframe, Maybe Keyframe)
findKeyframePair [] _ = (Nothing, Nothing)
findKeyframePair [kf] _ = (Just kf, Nothing)
findKeyframePair kfs time
  | time <= keyTime (head sortedKfs) = (Just (head sortedKfs), Just (head sortedKfs))
  | time >= keyTime (last sortedKfs) = (Just (last sortedKfs), Just (last sortedKfs))
  | otherwise = findPair sortedKfs
  where
    sortedKfs = sortKeyframes kfs

    findPair (k1:k2:rest)
      | time >= keyTime k1 && time <= keyTime k2 = (Just k1, Just k2)
      | otherwise = findPair (k2:rest)
    findPair [k] = (Just k, Nothing)
    findPair [] = (Nothing, Nothing)

-- | Clamp a value between min and max
clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = max minVal (min maxVal x)

-- | Remap a value from one range to another
remap :: Float -> Float -> Float -> Float -> Float -> Float
remap inMin inMax outMin outMax x =
  let normalized = (x - inMin) / (inMax - inMin)
  in outMin + normalized * (outMax - outMin)
