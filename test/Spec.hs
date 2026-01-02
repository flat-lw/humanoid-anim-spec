module Main (main) where

import Test.Hspec

import qualified Motion.InterpolationSpec

main :: IO ()
main = hspec $ do
  describe "Motion.Interpolation" Motion.InterpolationSpec.spec
