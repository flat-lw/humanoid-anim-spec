-- |
-- Module      : HumanoidAnim.Motion.Easing
-- Description : Easing functions for animation
--
-- This module provides various easing functions for smooth animation
-- interpolation. All functions take a normalized time value (0-1) and
-- return a normalized output value (0-1).
module HumanoidAnim.Motion.Easing
  ( -- * Easing Type
    Easing(..)

    -- * Applying Easing
  , applyEasing

    -- * Parsing
  , parseEasing
  , easingName
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Easing function type
data Easing
  = Linear
    -- ^ Linear interpolation (no easing)
  | EaseIn
    -- ^ Quadratic ease in (slow start)
  | EaseOut
    -- ^ Quadratic ease out (slow end)
  | EaseInOut
    -- ^ Quadratic ease in-out (slow start and end)
  | CubicIn
    -- ^ Cubic ease in
  | CubicOut
    -- ^ Cubic ease out
  | Cubic
    -- ^ Cubic ease in-out
  | QuartIn
    -- ^ Quartic ease in
  | QuartOut
    -- ^ Quartic ease out
  | QuartInOut
    -- ^ Quartic ease in-out
  | SineIn
    -- ^ Sinusoidal ease in
  | SineOut
    -- ^ Sinusoidal ease out
  | SineInOut
    -- ^ Sinusoidal ease in-out
  | ExpoIn
    -- ^ Exponential ease in
  | ExpoOut
    -- ^ Exponential ease out
  | ExpoInOut
    -- ^ Exponential ease in-out
  | Bounce
    -- ^ Bounce effect at end
  | Custom (Float -> Float)
    -- ^ Custom easing function

instance Show Easing where
  show Linear = "Linear"
  show EaseIn = "EaseIn"
  show EaseOut = "EaseOut"
  show EaseInOut = "EaseInOut"
  show CubicIn = "CubicIn"
  show CubicOut = "CubicOut"
  show Cubic = "Cubic"
  show QuartIn = "QuartIn"
  show QuartOut = "QuartOut"
  show QuartInOut = "QuartInOut"
  show SineIn = "SineIn"
  show SineOut = "SineOut"
  show SineInOut = "SineInOut"
  show ExpoIn = "ExpoIn"
  show ExpoOut = "ExpoOut"
  show ExpoInOut = "ExpoInOut"
  show Bounce = "Bounce"
  show (Custom _) = "Custom"

instance Eq Easing where
  Linear == Linear = True
  EaseIn == EaseIn = True
  EaseOut == EaseOut = True
  EaseInOut == EaseInOut = True
  CubicIn == CubicIn = True
  CubicOut == CubicOut = True
  Cubic == Cubic = True
  QuartIn == QuartIn = True
  QuartOut == QuartOut = True
  QuartInOut == QuartInOut = True
  SineIn == SineIn = True
  SineOut == SineOut = True
  SineInOut == SineInOut = True
  ExpoIn == ExpoIn = True
  ExpoOut == ExpoOut = True
  ExpoInOut == ExpoInOut = True
  Bounce == Bounce = True
  (Custom _) == (Custom _) = False  -- Can't compare functions
  _ == _ = False

-- | Apply an easing function to a normalized time value
applyEasing :: Easing -> Float -> Float
applyEasing Linear t = t
applyEasing EaseIn t = t * t
applyEasing EaseOut t = t * (2 - t)
applyEasing EaseInOut t
  | t < 0.5   = 2 * t * t
  | otherwise = -1 + (4 - 2 * t) * t
applyEasing CubicIn t = t * t * t
applyEasing CubicOut t = let t' = t - 1 in t' * t' * t' + 1
applyEasing Cubic t
  | t < 0.5   = 4 * t * t * t
  | otherwise = let t' = 2 * t - 2 in (t' * t' * t' + 2) / 2
applyEasing QuartIn t = t * t * t * t
applyEasing QuartOut t = let t' = t - 1 in 1 - t' * t' * t' * t'
applyEasing QuartInOut t
  | t < 0.5   = 8 * t * t * t * t
  | otherwise = let t' = t - 1 in 1 - 8 * t' * t' * t' * t'
applyEasing SineIn t = 1 - cos (t * pi / 2)
applyEasing SineOut t = sin (t * pi / 2)
applyEasing SineInOut t = (1 - cos (t * pi)) / 2
applyEasing ExpoIn t
  | t == 0    = 0
  | otherwise = 2 ** (10 * (t - 1))
applyEasing ExpoOut t
  | t == 1    = 1
  | otherwise = 1 - 2 ** (-10 * t)
applyEasing ExpoInOut t
  | t == 0    = 0
  | t == 1    = 1
  | t < 0.5   = 2 ** (20 * t - 10) / 2
  | otherwise = (2 - 2 ** (-20 * t + 10)) / 2
applyEasing Bounce t = bounceOut t
applyEasing (Custom f) t = f t

-- | Bounce easing helper
bounceOut :: Float -> Float
bounceOut t
  | t < 1 / 2.75     = 7.5625 * t * t
  | t < 2 / 2.75     = let t' = t - 1.5 / 2.75 in 7.5625 * t' * t' + 0.75
  | t < 2.5 / 2.75   = let t' = t - 2.25 / 2.75 in 7.5625 * t' * t' + 0.9375
  | otherwise        = let t' = t - 2.625 / 2.75 in 7.5625 * t' * t' + 0.984375

-- | Parse easing from text (case-insensitive)
parseEasing :: Text -> Maybe Easing
parseEasing txt = case T.toLower txt of
  "linear"      -> Just Linear
  "easein"      -> Just EaseIn
  "ease-in"     -> Just EaseIn
  "easeout"     -> Just EaseOut
  "ease-out"    -> Just EaseOut
  "easeinout"   -> Just EaseInOut
  "ease-in-out" -> Just EaseInOut
  "cubicin"     -> Just CubicIn
  "cubic-in"    -> Just CubicIn
  "cubicout"    -> Just CubicOut
  "cubic-out"   -> Just CubicOut
  "cubic"       -> Just Cubic
  "quartin"     -> Just QuartIn
  "quart-in"    -> Just QuartIn
  "quartout"    -> Just QuartOut
  "quart-out"   -> Just QuartOut
  "quartinout"  -> Just QuartInOut
  "quart-in-out"-> Just QuartInOut
  "sinein"      -> Just SineIn
  "sine-in"     -> Just SineIn
  "sineout"     -> Just SineOut
  "sine-out"    -> Just SineOut
  "sineinout"   -> Just SineInOut
  "sine-in-out" -> Just SineInOut
  "expoin"      -> Just ExpoIn
  "expo-in"     -> Just ExpoIn
  "expoout"     -> Just ExpoOut
  "expo-out"    -> Just ExpoOut
  "expoinout"   -> Just ExpoInOut
  "expo-in-out" -> Just ExpoInOut
  "bounce"      -> Just Bounce
  _             -> Nothing

-- | Get the standard name for an easing function
easingName :: Easing -> Text
easingName Linear = "linear"
easingName EaseIn = "easeIn"
easingName EaseOut = "easeOut"
easingName EaseInOut = "easeInOut"
easingName CubicIn = "cubicIn"
easingName CubicOut = "cubicOut"
easingName Cubic = "cubic"
easingName QuartIn = "quartIn"
easingName QuartOut = "quartOut"
easingName QuartInOut = "quartInOut"
easingName SineIn = "sineIn"
easingName SineOut = "sineOut"
easingName SineInOut = "sineInOut"
easingName ExpoIn = "expoIn"
easingName ExpoOut = "expoOut"
easingName ExpoInOut = "expoInOut"
easingName Bounce = "bounce"
easingName (Custom _) = "custom"
