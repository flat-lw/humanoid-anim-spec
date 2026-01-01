-- |
-- Module      : HumanoidAnim.Input.Validation
-- Description : Configuration validation
--
-- This module provides validation functions for animation configuration,
-- checking for errors and generating warnings for potential issues.
module HumanoidAnim.Input.Validation
  ( -- * Validation
    validateConfig
  , validateFixedBones
  , validateEffector
  , validateKeyframes

    -- * Conversion
  , configToConstraints
  , configToKeyframes
  ) where

import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Linear (V3(..), Quaternion(..))

import HumanoidAnim.Error
import HumanoidAnim.IK.Core (IKConstraint(..))
import HumanoidAnim.Input.Config
import HumanoidAnim.Motion.Keyframe (Keyframe(..))
import HumanoidAnim.Skeleton.Bones (HumanoidBone, parseBoneName)
import HumanoidAnim.Skeleton.Transform (convertZUpToYUp)

-- | Validate entire configuration
validateConfig :: AnimationConfig -> Result AnimationConfig
validateConfig config = do
  -- Validate duration
  validateDuration (configDuration config)

  -- Validate fixed bones
  _ <- validateFixedBones (configFixed config)

  -- Validate effector
  _ <- validateEffector (configEffector config)

  -- Add coordinate system warning if needed
  let result = success config
  if inCoordinateSystem (configInput config) == "z-up"
    then addWarning (CoordinateSystemConverted "z-up" "y-up") result
    else result

-- | Validate duration
validateDuration :: Float -> Result ()
validateDuration d
  | d <= 0 = failure $ InvalidDuration d
  | otherwise = success ()

-- | Validate fixed bone configurations
validateFixedBones :: [FixedBoneConfig] -> Result [HumanoidBone]
validateFixedBones [] = failure $ MissingField "fixed (at least one fixed bone required)"
validateFixedBones configs = do
  bones <- mapM validateFixedBone configs
  success bones

-- | Validate a single fixed bone
validateFixedBone :: FixedBoneConfig -> Result HumanoidBone
validateFixedBone cfg =
  case parseBoneName (fixedBone cfg) of
    Just bone -> success bone
    Nothing -> failure $ InvalidBoneName (T.unpack $ fixedBone cfg)

-- | Validate effector configuration
validateEffector :: EffectorConfig -> Result HumanoidBone
validateEffector cfg = do
  -- Validate bone name
  bone <- case parseBoneName (effectorBone cfg) of
    Just b -> success b
    Nothing -> failure $ InvalidBoneName (T.unpack $ effectorBone cfg)

  -- Validate keyframes
  _ <- validateKeyframes (effectorKeyframes cfg)

  success bone

-- | Validate keyframe list
validateKeyframes :: [KeyframeConfig] -> Result [KeyframeConfig]
validateKeyframes kfs
  | length kfs < 2 = failure $ InsufficientKeyframes (length kfs)
  | otherwise = do
      -- Check time ordering
      let sorted = sortBy (comparing kfTime) kfs
          pairs = zip sorted (tail sorted)
          outOfOrder = [(k1, k2) | (k1, k2) <- pairs, kfTime k1 > kfTime k2]
      case outOfOrder of
        ((k1, k2):_) -> failure $ TimeOrderError (kfTime k1) (kfTime k2)
        [] -> success kfs

-- | Convert configuration to IK constraints
configToConstraints :: AnimationConfig -> Result [IKConstraint]
configToConstraints config = do
  let coordConvert = if inCoordinateSystem (configInput config) == "z-up"
                     then convertZUpToYUp
                     else id

  -- Convert fixed bones to constraints
  fixedConstraints <- mapM (toFixedConstraint coordConvert) (configFixed config)

  success fixedConstraints

-- | Convert a fixed bone config to an IK constraint
toFixedConstraint :: (V3 Float -> V3 Float) -> FixedBoneConfig -> Result IKConstraint
toFixedConstraint coordConvert cfg =
  case parseBoneName (fixedBone cfg) of
    Just bone ->
      let pos = coordConvert $ fixedPosition cfg
      in case fixedRotation cfg of
           Just rot -> success $ FixedWithRotation bone pos rot
           Nothing -> success $ Fixed bone pos
    Nothing -> failure $ InvalidBoneName (T.unpack $ fixedBone cfg)

-- | Convert effector keyframes to Keyframe type
configToKeyframes :: AnimationConfig -> Result [Keyframe]
configToKeyframes config = do
  let coordConvert = if inCoordinateSystem (configInput config) == "z-up"
                     then convertZUpToYUp
                     else id
      kfConfigs = effectorKeyframes (configEffector config)

  success $ map (toKeyframe coordConvert) kfConfigs

-- | Convert a keyframe config to Keyframe
toKeyframe :: (V3 Float -> V3 Float) -> KeyframeConfig -> Keyframe
toKeyframe coordConvert cfg = Keyframe
  { keyTime = kfTime cfg
  , keyPosition = coordConvert (kfPosition cfg)
  , keyRotation = kfRotation cfg  -- TODO: rotate quaternion too if z-up
  , keyEasing = kfEasing cfg
  }
