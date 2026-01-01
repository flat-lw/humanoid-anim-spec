-- |
-- Module      : HumanoidAnim.Skeleton.Bones
-- Description : Humanoid bone definitions
--
-- This module defines the humanoid bone enumeration following Unity's
-- Humanoid rig specification, along with bone categories and utilities.
module HumanoidAnim.Skeleton.Bones
  ( -- * Bone Type
    HumanoidBone(..)

    -- * Bone Categories
  , BoneCategory(..)
  , SkeletonDetail(..)
  , boneCategory
  , bonesUpTo
  , bonesForDetail

    -- * Bone Queries
  , allBones
  , requiredBones
  , recommendedBones
  , optionalBones

    -- * Bone Name Conversion
  , boneName
  , parseBoneName
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Humanoid bone enumeration
-- Following Unity's Humanoid rig specification
data HumanoidBone
  -- Required bones (15)
  = Hips
  | Spine
  | Chest
  | Neck
  | Head
  | LeftUpperArm
  | LeftLowerArm
  | LeftHand
  | RightUpperArm
  | RightLowerArm
  | RightHand
  | LeftUpperLeg
  | LeftLowerLeg
  | LeftFoot
  | RightUpperLeg
  | RightLowerLeg
  | RightFoot

  -- Recommended bones (additional 6 = total 21)
  | UpperChest
  | LeftShoulder
  | RightShoulder
  | LeftToes
  | RightToes
  | Jaw

  -- Full skeleton: Fingers (additional 30)
  | LeftThumbProximal
  | LeftThumbIntermediate
  | LeftThumbDistal
  | LeftIndexProximal
  | LeftIndexIntermediate
  | LeftIndexDistal
  | LeftMiddleProximal
  | LeftMiddleIntermediate
  | LeftMiddleDistal
  | LeftRingProximal
  | LeftRingIntermediate
  | LeftRingDistal
  | LeftLittleProximal
  | LeftLittleIntermediate
  | LeftLittleDistal
  | RightThumbProximal
  | RightThumbIntermediate
  | RightThumbDistal
  | RightIndexProximal
  | RightIndexIntermediate
  | RightIndexDistal
  | RightMiddleProximal
  | RightMiddleIntermediate
  | RightMiddleDistal
  | RightRingProximal
  | RightRingIntermediate
  | RightRingDistal
  | RightLittleProximal
  | RightLittleIntermediate
  | RightLittleDistal

  -- Full skeleton: Other (additional 4)
  | LeftEye
  | RightEye
  | UpperChestTwist
  | SpineTwist
  deriving stock (Show, Eq, Ord, Enum, Bounded, Read)

-- | Bone category classification
data BoneCategory
  = Required     -- ^ Must be present for valid humanoid
  | Recommended  -- ^ Should be present for best results
  | Optional     -- ^ Nice to have for detailed animation
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Skeleton detail level
data SkeletonDetail
  = MinimalSkeleton   -- ^ Required bones only (15)
  | StandardSkeleton  -- ^ Required + Recommended (21)
  | FullSkeleton      -- ^ All bones (55)
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded)

-- | Get the category of a bone
boneCategory :: HumanoidBone -> BoneCategory
boneCategory bone
  | bone `elem` requiredBones = Required
  | bone `elem` recommendedBones = Recommended
  | otherwise = Optional

-- | Get all bones up to and including a category
bonesUpTo :: BoneCategory -> [HumanoidBone]
bonesUpTo Required = requiredBones
bonesUpTo Recommended = requiredBones ++ recommendedBones
bonesUpTo Optional = allBones

-- | Get bones for a skeleton detail level
bonesForDetail :: SkeletonDetail -> [HumanoidBone]
bonesForDetail MinimalSkeleton = requiredBones
bonesForDetail StandardSkeleton = requiredBones ++ recommendedBones
bonesForDetail FullSkeleton = allBones

-- | All defined bones
allBones :: [HumanoidBone]
allBones = [minBound .. maxBound]

-- | Required bones (15)
requiredBones :: [HumanoidBone]
requiredBones =
  [ Hips, Spine, Chest, Neck, Head
  , LeftUpperArm, LeftLowerArm, LeftHand
  , RightUpperArm, RightLowerArm, RightHand
  , LeftUpperLeg, LeftLowerLeg, LeftFoot
  , RightUpperLeg, RightLowerLeg, RightFoot
  ]

-- | Recommended bones (6)
recommendedBones :: [HumanoidBone]
recommendedBones =
  [ UpperChest
  , LeftShoulder, RightShoulder
  , LeftToes, RightToes
  , Jaw
  ]

-- | Optional bones (fingers and extras)
optionalBones :: [HumanoidBone]
optionalBones =
  [ LeftThumbProximal, LeftThumbIntermediate, LeftThumbDistal
  , LeftIndexProximal, LeftIndexIntermediate, LeftIndexDistal
  , LeftMiddleProximal, LeftMiddleIntermediate, LeftMiddleDistal
  , LeftRingProximal, LeftRingIntermediate, LeftRingDistal
  , LeftLittleProximal, LeftLittleIntermediate, LeftLittleDistal
  , RightThumbProximal, RightThumbIntermediate, RightThumbDistal
  , RightIndexProximal, RightIndexIntermediate, RightIndexDistal
  , RightMiddleProximal, RightMiddleIntermediate, RightMiddleDistal
  , RightRingProximal, RightRingIntermediate, RightRingDistal
  , RightLittleProximal, RightLittleIntermediate, RightLittleDistal
  , LeftEye, RightEye
  , UpperChestTwist, SpineTwist
  ]

-- | Convert bone to standard name (for config files)
boneName :: HumanoidBone -> Text
boneName = T.pack . show

-- | Parse bone name from text (case-insensitive)
parseBoneName :: Text -> Maybe HumanoidBone
parseBoneName txt =
  let normalizedInput = T.toLower txt
      matches = mapMaybe matchBone allBones
      matchBone bone =
        if T.toLower (boneName bone) == normalizedInput
          then Just bone
          else Nothing
  in case matches of
       (b:_) -> Just b
       [] -> Nothing
