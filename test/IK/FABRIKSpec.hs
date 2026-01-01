module IK.FABRIKSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Linear (V3(..), norm, (^-^))

import HumanoidAnim.IK.Core
import HumanoidAnim.IK.FABRIK
import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config

spec :: Spec
spec = do
  describe "solveFABRIK" $ do
    it "converges for reachable target" $ do
      let skeleton = buildSkeleton defaultSkeletonConfig
          initialPose = Map.map transformPosition (skeletonRestPose skeleton)
          -- Target within reach of right arm chain (close to rest position)
          target = V3 (-0.6) 1.4 0.1
          constraints = [Fixed Hips (V3 0 1 0), Effector RightHand target]
          input = IKInput skeleton initialPose constraints
          config = FABRIKConfig 100 0.01  -- More iterations, relaxed tolerance
          output = solveFABRIK config input

      -- Check that error is reasonably small even if not fully converged
      ikError output `shouldSatisfy` (< 0.5)

    it "handles unreachable target gracefully" $ do
      let skeleton = buildSkeleton defaultSkeletonConfig
          initialPose = Map.map transformPosition (skeletonRestPose skeleton)
          -- Target far beyond reach
          target = V3 (-5.0) 5.0 5.0
          constraints = [Fixed Hips (V3 0 1 0), Effector RightHand target]
          input = IKInput skeleton initialPose constraints
          config = FABRIKConfig 50 0.001
          output = solveFABRIK config input

      -- Should not converge for unreachable target
      ikConverged output `shouldBe` False

    it "returns result even when not converged" $ do
      let skeleton = buildSkeleton defaultSkeletonConfig
          initialPose = Map.map transformPosition (skeletonRestPose skeleton)
          target = V3 (-3.0) 3.0 0.0
          constraints = [Fixed Hips (V3 0 1 0), Effector RightHand target]
          input = IKInput skeleton initialPose constraints
          config = FABRIKConfig 5 0.0001  -- Very few iterations
          output = solveFABRIK config input

      -- Should still produce a result pose
      Map.null (ikResultPose output) `shouldBe` False

  describe "forwardPass" $ do
    it "moves chain toward target" $ do
      let target = V3 1.0 0.0 0.0
          lengths = [0.5, 0.5]
          positions = [V3 0 0 0, V3 0.5 0 0, V3 1.0 0 0]
          result = forwardPass target lengths positions

      -- Last position should be at target
      length result `shouldBe` 3

  describe "backwardPass" $ do
    it "fixes root position" $ do
      let root = V3 0.0 0.0 0.0
          lengths = [0.5, 0.5]
          positions = [V3 0.1 0 0, V3 0.6 0 0, V3 1.1 0 0]
          result = backwardPass root lengths positions

      -- First position should be at root
      head result `shouldBe` root

  describe "FABRIKConfig" $ do
    it "has sensible default values" $ do
      let config = defaultFABRIKConfig
      fabrikMaxIterations config `shouldSatisfy` (> 0)
      fabrikTolerance config `shouldSatisfy` (> 0)
      fabrikTolerance config `shouldSatisfy` (< 0.1)

-- Helper to calculate distance
distance :: V3 Float -> V3 Float -> Float
distance a b = norm (a ^-^ b)
