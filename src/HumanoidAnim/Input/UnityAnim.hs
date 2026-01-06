-- |
-- Module      : HumanoidAnim.Input.UnityAnim
-- Description : Unity .anim file format parser
--
-- This module parses Unity's native .anim format (YAML-based),
-- extracting animation curves for muscles and transforms.
module HumanoidAnim.Input.UnityAnim
  ( -- * Parsing Functions
    parseUnityAnim
  , loadUnityAnim

    -- * Types
  , ParsedAnimClip(..)
  , ParsedCurve(..)
  , ParsedKeyframe(..)

    -- * Utilities
  , getMuscleValue
  , getMuscleCurve
  , getAllMuscleNames
  , sampleAtTime
  ) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (isPrefixOf, find, sortBy)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HumanoidAnim.Error

-- | Parsed animation clip from Unity .anim file
data ParsedAnimClip = ParsedAnimClip
  { pacName :: String
    -- ^ Animation clip name
  , pacSampleRate :: Float
    -- ^ Sample rate (fps)
  , pacDuration :: Float
    -- ^ Total duration in seconds
  , pacFloatCurves :: [ParsedCurve]
    -- ^ Float curves (muscles, blend shapes, etc.)
  , pacPositionCurves :: [ParsedCurve]
    -- ^ Position curves (RootT, etc.)
  , pacRotationCurves :: [ParsedCurve]
    -- ^ Rotation curves (RootQ, etc.)
  } deriving stock (Show, Eq)

-- | Parsed animation curve
data ParsedCurve = ParsedCurve
  { pcAttribute :: String
    -- ^ Attribute name (e.g., "Right Arm Down-Up", "RootT.x")
  , pcPath :: String
    -- ^ Path in hierarchy (empty for muscle curves)
  , pcKeyframes :: [ParsedKeyframe]
    -- ^ Keyframe data
  } deriving stock (Show, Eq)

-- | Parsed keyframe
data ParsedKeyframe = ParsedKeyframe
  { pkTime :: Float
  , pkValue :: Float
  , pkInSlope :: Float
  , pkOutSlope :: Float
  , pkInWeight :: Float
  , pkOutWeight :: Float
  } deriving stock (Show, Eq)

-- | Load and parse a Unity .anim file
loadUnityAnim :: FilePath -> IO (Result ParsedAnimClip)
loadUnityAnim path = do
  content <- TIO.readFile path
  return $ parseUnityAnim (T.unpack content)

-- | Parse Unity .anim content
parseUnityAnim :: String -> Result ParsedAnimClip
parseUnityAnim content =
  let ls = lines content
  in case parseAnimClip ls of
    Just clip -> success clip
    Nothing -> failure $ ConfigParseError "input" "Failed to parse Unity .anim file"

-- | Parse the AnimationClip from lines
parseAnimClip :: [String] -> Maybe ParsedAnimClip
parseAnimClip ls = do
  -- Find clip name
  name <- findValue "m_Name:" ls

  -- Find sample rate (in m_EditorCurves or default to 60)
  let sampleRate = fromMaybe 60 $ findFloatValue "m_SampleRate:" ls

  -- Parse m_FloatCurves section
  let floatCurves = parseFloatCurves ls

  -- Parse position/rotation curves if present
  let posCurves = parsePositionCurves ls
  let rotCurves = parseRotationCurves ls

  -- Calculate duration from max keyframe time
  let allKeyframes = concatMap pcKeyframes (floatCurves ++ posCurves ++ rotCurves)
  let duration = if null allKeyframes
                 then 1.0
                 else maximum $ map pkTime allKeyframes

  Just ParsedAnimClip
    { pacName = name
    , pacSampleRate = sampleRate
    , pacDuration = duration
    , pacFloatCurves = floatCurves
    , pacPositionCurves = posCurves
    , pacRotationCurves = rotCurves
    }

-- | Parse m_FloatCurves section (muscle curves)
parseFloatCurves :: [String] -> [ParsedCurve]
parseFloatCurves ls =
  let -- Find lines between m_FloatCurves: and the next major section
      inFloatSection = dropWhile (not . isPrefixOf "  m_FloatCurves:") ls
  in case inFloatSection of
    [] -> []
    (_:rest) -> parseCurveList rest

-- | Parse position curves (m_PositionCurves)
parsePositionCurves :: [String] -> [ParsedCurve]
parsePositionCurves ls =
  let inSection = dropWhile (not . isPrefixOf "  m_PositionCurves:") ls
  in case inSection of
    [] -> []
    (_:rest) -> parseCurveList rest

-- | Parse rotation curves (m_RotationCurves)
parseRotationCurves :: [String] -> [ParsedCurve]
parseRotationCurves ls =
  let inSection = dropWhile (not . isPrefixOf "  m_RotationCurves:") ls
  in case inSection of
    [] -> []
    (_:rest) -> parseCurveList rest

-- | Parse a list of curves until a new section starts
parseCurveList :: [String] -> [ParsedCurve]
parseCurveList ls = go ls []
  where
    go [] acc = reverse acc
    go (l:rest) acc
      -- New curve entry starts with "  - curve:"
      | "  - curve:" `isPrefixOf` l =
          case parseSingleCurve (l:rest) of
            (Just curve, remaining) -> go remaining (curve : acc)
            (Nothing, remaining) -> go remaining acc
      -- End of section (new major section or end)
      | not (startsWithSpaces l 4) && not (null (dropWhile isSpace l)) =
          reverse acc
      | otherwise = go rest acc

    startsWithSpaces s n = length (takeWhile isSpace s) >= n

-- | Parse a single curve entry
-- Unity .anim structure:
--   - curve:
--       m_Curve:
--       - serializedVersion: 3
--         time: 0.0
--         value: 0.5
--         ...
--     attribute: "Muscle Name"
--     path: ""
--     ...
parseSingleCurve :: [String] -> (Maybe ParsedCurve, [String])
parseSingleCurve [] = (Nothing, [])
parseSingleCurve (l:ls)
  | "  - curve:" `isPrefixOf` l =
      let -- Collect all lines belonging to this curve entry
          -- A curve entry ends when we see the next "  - curve:" or a section end
          (curveLines, rest) = collectCurveLines ls

          -- Parse keyframes from the m_Curve section within curveLines
          keyframes = parseKeyframes curveLines

          -- Find attribute name (comes after the keyframes in Unity format)
          attribute = findAttributeInLines curveLines

          -- Find path
          path = fromMaybe "" $ findPathInLines curveLines

      in case attribute of
        Just attr -> (Just ParsedCurve
          { pcAttribute = attr
          , pcPath = path
          , pcKeyframes = keyframes
          }, rest)
        Nothing -> (Nothing, rest)
  | otherwise = (Nothing, l:ls)
  where
    -- Collect lines until we hit the next curve entry or section end
    collectCurveLines :: [String] -> ([String], [String])
    collectCurveLines xs = span isCurveLine xs

    isCurveLine x =
      let stripped = dropWhile isSpace x
      in null stripped ||
         not ("  - curve:" `isPrefixOf` x) &&
         (startsWithSpaces x 4 || startsWithSpaces x 6 || startsWithSpaces x 8 || startsWithSpaces x 10)

    startsWithSpaces s n = length (takeWhile isSpace s) >= n

-- | Parse keyframes from curve lines
parseKeyframes :: [String] -> [ParsedKeyframe]
parseKeyframes ls = go ls []
  where
    go [] acc = reverse acc
    go (l:rest) acc
      | "        time:" `isPrefixOf` l =
          case parseKeyframe (l:rest) of
            (Just kf, remaining) -> go remaining (kf : acc)
            (Nothing, remaining) -> go remaining acc
      | otherwise = go rest acc

-- | Parse a single keyframe
parseKeyframe :: [String] -> (Maybe ParsedKeyframe, [String])
parseKeyframe ls =
  let (kfLines, rest) = span isKeyframeLine ls

      time = findFloatValue "time:" kfLines
      value = findFloatValue "value:" kfLines
      inSlope = fromMaybe 0 $ findFloatValue "inSlope:" kfLines
      outSlope = fromMaybe 0 $ findFloatValue "outSlope:" kfLines
      inWeight = fromMaybe 0.333333 $ findFloatValue "inWeight:" kfLines
      outWeight = fromMaybe 0.333333 $ findFloatValue "outWeight:" kfLines

  in case (time, value) of
    (Just t, Just v) -> (Just ParsedKeyframe
      { pkTime = t
      , pkValue = v
      , pkInSlope = inSlope
      , pkOutSlope = outSlope
      , pkInWeight = inWeight
      , pkOutWeight = outWeight
      }, rest)
    _ -> (Nothing, rest)
  where
    isKeyframeLine l =
      let spaces = length (takeWhile isSpace l)
      in spaces >= 8 && not ("      - serializedVersion:" `isPrefixOf` l && spaces == 6)

-- | Find attribute name in curve lines
findAttributeInLines :: [String] -> Maybe String
findAttributeInLines = findValue "attribute:"

-- | Find path in curve lines
findPathInLines :: [String] -> Maybe String
findPathInLines ls =
  case findValue "path:" ls of
    Just "" -> Nothing
    Just p -> Just p
    Nothing -> Nothing

-- | Find a string value after a key
findValue :: String -> [String] -> Maybe String
findValue key ls =
  case find (key `isSubstringOf`) ls of
    Just l ->
      let afterKey = dropWhile (/= ':') l
          value = drop 1 afterKey  -- drop the ':'
          trimmed = dropWhile isSpace value
      in Just trimmed
    Nothing -> Nothing
  where
    isSubstringOf needle haystack = needle `isPrefixOf` (dropWhile isSpace haystack)

-- | Find a float value after a key
findFloatValue :: String -> [String] -> Maybe Float
findFloatValue key ls =
  case findValue key ls of
    Just s -> readMaybeFloat s
    Nothing -> Nothing

-- | Safe read for Float
readMaybeFloat :: String -> Maybe Float
readMaybeFloat s =
  case reads s of
    [(f, _)] -> Just f
    _ -> Nothing

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Get muscle value at a specific time from a parsed clip
getMuscleValue :: ParsedAnimClip -> String -> Float -> Maybe Float
getMuscleValue clip muscleName time =
  case getMuscleCurve clip muscleName of
    Just curve -> Just $ sampleCurveAtTime curve time
    Nothing -> Nothing

-- | Get a specific muscle curve by name
getMuscleCurve :: ParsedAnimClip -> String -> Maybe ParsedCurve
getMuscleCurve clip muscleName =
  find (\c -> pcAttribute c == muscleName) (pacFloatCurves clip)

-- | Get all muscle names in the clip
getAllMuscleNames :: ParsedAnimClip -> [String]
getAllMuscleNames clip = map pcAttribute (pacFloatCurves clip)

-- | Sample a curve at a specific time using linear interpolation
sampleCurveAtTime :: ParsedCurve -> Float -> Float
sampleCurveAtTime curve time =
  let kfs = sortBy (comparing pkTime) (pcKeyframes curve)
  in case kfs of
    [] -> 0
    [kf] -> pkValue kf
    _ ->
      -- Find surrounding keyframes
      let before = filter (\kf -> pkTime kf <= time) kfs
          after = filter (\kf -> pkTime kf > time) kfs
      in case (before, after) of
        ([], a:_) -> pkValue a  -- Before first keyframe
        (b, []) -> pkValue (last b)  -- After last keyframe
        (b, a:_) ->
          let kf1 = last b
              kf2 = a
              t = if pkTime kf2 == pkTime kf1
                  then 0
                  else (time - pkTime kf1) / (pkTime kf2 - pkTime kf1)
          in pkValue kf1 + t * (pkValue kf2 - pkValue kf1)

-- | Sample all muscles at a specific time
sampleAtTime :: ParsedAnimClip -> Float -> [(String, Float)]
sampleAtTime clip time =
  [(pcAttribute c, sampleCurveAtTime c time) | c <- pacFloatCurves clip]
