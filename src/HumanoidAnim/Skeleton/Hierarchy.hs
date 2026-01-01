-- |
-- Module      : HumanoidAnim.Skeleton.Hierarchy
-- Description : Bone parent-child relationships
--
-- This module defines the hierarchical structure of humanoid bones,
-- including parent-child relationships and chain extraction.
module HumanoidAnim.Skeleton.Hierarchy
  ( -- * Parent Relationships
    boneParent
  , boneChildren

    -- * Chain Extraction
  , getBoneChain
  , getChainBetween

    -- * Hierarchy Queries
  , isAncestor
  , isDescendant
  , getDepth
  , getRoot
  ) where

import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)

import HumanoidAnim.Skeleton.Bones

-- | Get the parent of a bone (Nothing for Hips which is root)
boneParent :: HumanoidBone -> Maybe HumanoidBone
boneParent = \case
  -- Root
  Hips -> Nothing

  -- Spine chain
  Spine -> Just Hips
  Chest -> Just Spine
  UpperChest -> Just Chest
  Neck -> Just UpperChest  -- Falls back to Chest if UpperChest not present
  Head -> Just Neck

  -- Left arm
  LeftShoulder -> Just UpperChest
  LeftUpperArm -> Just LeftShoulder  -- Falls back to UpperChest/Chest
  LeftLowerArm -> Just LeftUpperArm
  LeftHand -> Just LeftLowerArm

  -- Right arm
  RightShoulder -> Just UpperChest
  RightUpperArm -> Just RightShoulder
  RightLowerArm -> Just RightUpperArm
  RightHand -> Just RightLowerArm

  -- Left leg
  LeftUpperLeg -> Just Hips
  LeftLowerLeg -> Just LeftUpperLeg
  LeftFoot -> Just LeftLowerLeg
  LeftToes -> Just LeftFoot

  -- Right leg
  RightUpperLeg -> Just Hips
  RightLowerLeg -> Just RightUpperLeg
  RightFoot -> Just RightLowerLeg
  RightToes -> Just RightFoot

  -- Face
  Jaw -> Just Head
  LeftEye -> Just Head
  RightEye -> Just Head

  -- Left hand fingers
  LeftThumbProximal -> Just LeftHand
  LeftThumbIntermediate -> Just LeftThumbProximal
  LeftThumbDistal -> Just LeftThumbIntermediate

  LeftIndexProximal -> Just LeftHand
  LeftIndexIntermediate -> Just LeftIndexProximal
  LeftIndexDistal -> Just LeftIndexIntermediate

  LeftMiddleProximal -> Just LeftHand
  LeftMiddleIntermediate -> Just LeftMiddleProximal
  LeftMiddleDistal -> Just LeftMiddleIntermediate

  LeftRingProximal -> Just LeftHand
  LeftRingIntermediate -> Just LeftRingProximal
  LeftRingDistal -> Just LeftRingIntermediate

  LeftLittleProximal -> Just LeftHand
  LeftLittleIntermediate -> Just LeftLittleProximal
  LeftLittleDistal -> Just LeftLittleIntermediate

  -- Right hand fingers
  RightThumbProximal -> Just RightHand
  RightThumbIntermediate -> Just RightThumbProximal
  RightThumbDistal -> Just RightThumbIntermediate

  RightIndexProximal -> Just RightHand
  RightIndexIntermediate -> Just RightIndexProximal
  RightIndexDistal -> Just RightIndexIntermediate

  RightMiddleProximal -> Just RightHand
  RightMiddleIntermediate -> Just RightMiddleProximal
  RightMiddleDistal -> Just RightMiddleIntermediate

  RightRingProximal -> Just RightHand
  RightRingIntermediate -> Just RightRingProximal
  RightRingDistal -> Just RightRingIntermediate

  RightLittleProximal -> Just RightHand
  RightLittleIntermediate -> Just RightLittleProximal
  RightLittleDistal -> Just RightLittleIntermediate

  -- Twist bones
  UpperChestTwist -> Just UpperChest
  SpineTwist -> Just Spine

-- | Get immediate children of a bone
boneChildren :: HumanoidBone -> [HumanoidBone]
boneChildren parent =
  [ bone | bone <- allBones, boneParent bone == Just parent ]

-- | Get the chain of bones from root to the given bone (inclusive)
getBoneChain :: HumanoidBone -> [HumanoidBone]
getBoneChain bone = reverse $ bone : unfoldr step bone
  where
    step b = case boneParent b of
      Just p  -> Just (p, p)
      Nothing -> Nothing

-- | Get the chain of bones between two bones (if connected)
-- Returns Nothing if bones are not in same chain
getChainBetween :: HumanoidBone -> HumanoidBone -> Maybe [HumanoidBone]
getChainBetween from to
  | from == to = Just [from]
  | isAncestor from to = Just $ takeWhileIncluding (/= to) (drop 1 $ dropWhile (/= from) chain)
  | isAncestor to from = Just $ reverse $ takeWhileIncluding (/= from) (drop 1 $ dropWhile (/= to) reverseChain)
  | otherwise = Nothing
  where
    chain = getBoneChain to
    reverseChain = getBoneChain from
    takeWhileIncluding _ [] = []
    takeWhileIncluding p (x:xs)
      | p x = x : takeWhileIncluding p xs
      | otherwise = [x]

-- | Check if first bone is an ancestor of second
isAncestor :: HumanoidBone -> HumanoidBone -> Bool
isAncestor ancestor descendant
  | ancestor == descendant = False
  | otherwise = ancestor `elem` getBoneChain descendant

-- | Check if first bone is a descendant of second
isDescendant :: HumanoidBone -> HumanoidBone -> Bool
isDescendant = flip isAncestor

-- | Get the depth of a bone in the hierarchy (Hips = 0)
getDepth :: HumanoidBone -> Int
getDepth = length . getBoneChain

-- | Get the root bone (always Hips)
getRoot :: HumanoidBone
getRoot = Hips

-- | Parent map for efficient lookups
parentMap :: Map HumanoidBone (Maybe HumanoidBone)
parentMap = Map.fromList [(bone, boneParent bone) | bone <- allBones]
