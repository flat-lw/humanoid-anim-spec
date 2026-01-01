module Main (main) where

import Test.Hspec

import qualified IK.FABRIKSpec
import qualified Motion.InterpolationSpec

main :: IO ()
main = hspec $ do
  describe "IK.FABRIK" IK.FABRIKSpec.spec
  describe "Motion.Interpolation" Motion.InterpolationSpec.spec
