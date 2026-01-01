module Motion.InterpolationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Linear (V3(..), Quaternion(..), norm, (^-^))

import HumanoidAnim.Motion.Easing
import HumanoidAnim.Motion.Keyframe
import HumanoidAnim.Motion.Interpolation

spec :: Spec
spec = do
  describe "lerpV3" $ do
    it "returns start at t=0" $ do
      let start = V3 0 0 0
          end = V3 10 10 10
      lerpV3 start end 0 `shouldBe` start

    it "returns end at t=1" $ do
      let start = V3 0 0 0
          end = V3 10 10 10
      lerpV3 start end 1 `shouldBe` end

    it "returns midpoint at t=0.5" $ do
      let start = V3 0 0 0
          end = V3 10 10 10
      lerpV3 start end 0.5 `shouldBe` V3 5 5 5

  describe "slerpQuaternion" $ do
    it "returns first quaternion at t=0" $ do
      let q1 = Quaternion 1 (V3 0 0 0)
          q2 = Quaternion 0.707 (V3 0 0.707 0)
          result = slerpQuaternion q1 q2 0
      -- Should be approximately q1
      distance4 result q1 `shouldSatisfy` (< 0.001)

    it "returns second quaternion at t=1" $ do
      let q1 = Quaternion 1 (V3 0 0 0)
          q2 = Quaternion 0.707 (V3 0 0.707 0)
          result = slerpQuaternion q1 q2 1
      -- Should be approximately q2
      distance4 result q2 `shouldSatisfy` (< 0.01)

  describe "interpolateKeyframes" $ do
    it "returns exact position at keyframe time" $ do
      let kf1 = keyframe 0.0 (V3 0 0 0)
          kf2 = keyframe 1.0 (V3 10 10 10)
          keyframes = [kf1, kf2]
      interpolateKeyframes keyframes 0.0 `shouldBe` V3 0 0 0
      interpolateKeyframes keyframes 1.0 `shouldBe` V3 10 10 10

    it "interpolates linearly between keyframes" $ do
      let kf1 = keyframe 0.0 (V3 0 0 0)
          kf2 = keyframe 1.0 (V3 10 10 10)
          keyframes = [kf1, kf2]
      interpolateKeyframes keyframes 0.5 `shouldBe` V3 5 5 5

    it "clamps to first keyframe before start" $ do
      let kf1 = keyframe 1.0 (V3 5 5 5)
          kf2 = keyframe 2.0 (V3 10 10 10)
          keyframes = [kf1, kf2]
      interpolateKeyframes keyframes 0.0 `shouldBe` V3 5 5 5

    it "clamps to last keyframe after end" $ do
      let kf1 = keyframe 0.0 (V3 0 0 0)
          kf2 = keyframe 1.0 (V3 10 10 10)
          keyframes = [kf1, kf2]
      interpolateKeyframes keyframes 2.0 `shouldBe` V3 10 10 10

  describe "findKeyframePair" $ do
    it "finds correct pair for time between keyframes" $ do
      let kf1 = keyframe 0.0 (V3 0 0 0)
          kf2 = keyframe 1.0 (V3 5 5 5)
          kf3 = keyframe 2.0 (V3 10 10 10)
          keyframes = [kf1, kf2, kf3]
          (Just k1, Just k2) = findKeyframePair keyframes 0.5
      keyTime k1 `shouldBe` 0.0
      keyTime k2 `shouldBe` 1.0

  describe "applyEasing" $ do
    it "linear returns input unchanged" $ do
      applyEasing Linear 0.0 `shouldBe` 0.0
      applyEasing Linear 0.5 `shouldBe` 0.5
      applyEasing Linear 1.0 `shouldBe` 1.0

    it "easeIn starts slow" $ do
      applyEasing EaseIn 0.5 `shouldSatisfy` (< 0.5)

    it "easeOut ends slow" $ do
      applyEasing EaseOut 0.5 `shouldSatisfy` (> 0.5)

    it "all easing functions return 0 at t=0" $ do
      applyEasing Linear 0 `shouldBe` 0
      applyEasing EaseIn 0 `shouldBe` 0
      applyEasing EaseOut 0 `shouldBe` 0
      applyEasing EaseInOut 0 `shouldBe` 0
      applyEasing Cubic 0 `shouldBe` 0

    it "all easing functions return 1 at t=1" $ do
      applyEasing Linear 1 `shouldBe` 1
      applyEasing EaseIn 1 `shouldBe` 1
      applyEasing EaseOut 1 `shouldBe` 1
      applyEasing EaseInOut 1 `shouldBe` 1
      applyEasing Cubic 1 `shouldBe` 1

  describe "clamp" $ do
    it "clamps values below minimum" $ do
      clamp 0 10 (-5) `shouldBe` (0 :: Float)

    it "clamps values above maximum" $ do
      clamp 0 10 15 `shouldBe` (10 :: Float)

    it "leaves values in range unchanged" $ do
      clamp 0 10 5 `shouldBe` (5 :: Float)

-- Helper: quaternion distance
distance4 :: Quaternion Float -> Quaternion Float -> Float
distance4 (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  sqrt $ (w1-w2)^2 + (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2
