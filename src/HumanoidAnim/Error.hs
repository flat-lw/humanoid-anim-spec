-- |
-- Module      : HumanoidAnim.Error
-- Description : Error and warning types
--
-- This module defines error and warning types used throughout the
-- humanoid animation system, along with result handling utilities.
module HumanoidAnim.Error
  ( -- * Severity
    Severity(..)

    -- * Error Types
  , AppError(..)
  , errorCode
  , errorMessage

    -- * Warning Types
  , AppWarning(..)
  , warningCode
  , warningMessage

    -- * Result Type
  , Result(..)
  , success
  , failure
  , warning
  , addWarning
  , addWarnings
  , mapResult
  , bindResult

    -- * Error Formatting
  , formatError
  , formatWarning
  , formatResult
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import HumanoidAnim.Skeleton.Bones (HumanoidBone)

-- | Severity level for messages
data Severity = Info | Warning | Error
  deriving stock (Show, Eq, Ord)

-- | Application error types
data AppError
  = ConfigReadError FilePath String
    -- ^ E001: Failed to read configuration file
  | ConfigParseError FilePath String
    -- ^ E001: Failed to parse configuration file
  | MissingField String
    -- ^ E002: Required field missing
  | InvalidBoneName String
    -- ^ E003: Invalid bone name
  | InsufficientKeyframes Int
    -- ^ E004: Not enough keyframes (need at least 2)
  | TimeOrderError Float Float
    -- ^ E005: Keyframe times not in order
  | OutputWriteError FilePath String
    -- ^ E006: Failed to write output file
  | InvalidConstraint String
    -- ^ E007: Invalid IK constraint
  | InvalidDuration Float
    -- ^ E008: Invalid duration (must be positive)
  | InternalError String
    -- ^ E999: Internal error
  deriving stock (Show, Eq)

-- | Get error code
errorCode :: AppError -> Text
errorCode = \case
  ConfigReadError _ _     -> "E001"
  ConfigParseError _ _    -> "E001"
  MissingField _          -> "E002"
  InvalidBoneName _       -> "E003"
  InsufficientKeyframes _ -> "E004"
  TimeOrderError _ _      -> "E005"
  OutputWriteError _ _    -> "E006"
  InvalidConstraint _     -> "E007"
  InvalidDuration _       -> "E008"
  InternalError _         -> "E999"

-- | Get human-readable error message
errorMessage :: AppError -> Text
errorMessage = \case
  ConfigReadError path msg ->
    T.pack $ "Failed to read config file '" ++ path ++ "': " ++ msg
  ConfigParseError path msg ->
    T.pack $ "Failed to parse config file '" ++ path ++ "': " ++ msg
  MissingField field ->
    T.pack $ "Required field missing: " ++ field
  InvalidBoneName name ->
    T.pack $ "Invalid bone name: " ++ name
  InsufficientKeyframes n ->
    T.pack $ "At least 2 keyframes required, got " ++ show n
  TimeOrderError t1 t2 ->
    T.pack $ "Keyframe times out of order: " ++ show t1 ++ " > " ++ show t2
  OutputWriteError path msg ->
    T.pack $ "Failed to write output file '" ++ path ++ "': " ++ msg
  InvalidConstraint msg ->
    T.pack $ "Invalid IK constraint: " ++ msg
  InvalidDuration d ->
    T.pack $ "Invalid duration: " ++ show d ++ " (must be positive)"
  InternalError msg ->
    T.pack $ "Internal error: " ++ msg

-- | Application warning types
data AppWarning
  = UnreachableTarget HumanoidBone Float
    -- ^ W001: Target position unreachable
  | IKNotConverged Int Float
    -- ^ W002: IK solver did not converge
  | CoordinateSystemConverted String String
    -- ^ W003: Coordinate system converted
  | UnknownFieldIgnored String
    -- ^ W004: Unknown field in config ignored
  | ClampedValue String Float Float Float
    -- ^ W005: Value was clamped to valid range
  deriving stock (Show, Eq)

-- | Get warning code
warningCode :: AppWarning -> Text
warningCode = \case
  UnreachableTarget _ _       -> "W001"
  IKNotConverged _ _          -> "W002"
  CoordinateSystemConverted _ _ -> "W003"
  UnknownFieldIgnored _       -> "W004"
  ClampedValue _ _ _ _        -> "W005"

-- | Get human-readable warning message
warningMessage :: AppWarning -> Text
warningMessage = \case
  UnreachableTarget bone dist ->
    T.pack $ "Target for " ++ show bone ++ " is unreachable by " ++ show dist ++ "m"
  IKNotConverged iters err ->
    T.pack $ "IK solver did not converge after " ++ show iters ++
             " iterations (error: " ++ show err ++ "m)"
  CoordinateSystemConverted from to ->
    T.pack $ "Coordinate system converted from " ++ from ++ " to " ++ to
  UnknownFieldIgnored field ->
    T.pack $ "Unknown field ignored: " ++ field
  ClampedValue name val minV maxV ->
    T.pack $ name ++ " value " ++ show val ++
             " clamped to range [" ++ show minV ++ ", " ++ show maxV ++ "]"

-- | Result type with value and warnings
data Result a = Result
  { resultValue :: Either AppError a
  , resultWarnings :: [AppWarning]
  } deriving stock (Show, Eq)

-- | Create a successful result
success :: a -> Result a
success x = Result (Right x) []

-- | Create a failure result
failure :: AppError -> Result a
failure err = Result (Left err) []

-- | Create a successful result with a warning
warning :: a -> AppWarning -> Result a
warning x w = Result (Right x) [w]

-- | Add a warning to a result
addWarning :: AppWarning -> Result a -> Result a
addWarning w r = r { resultWarnings = w : resultWarnings r }

-- | Add multiple warnings to a result
addWarnings :: [AppWarning] -> Result a -> Result a
addWarnings ws r = r { resultWarnings = ws ++ resultWarnings r }

-- | Map over the value in a result
mapResult :: (a -> b) -> Result a -> Result b
mapResult f r = r { resultValue = fmap f (resultValue r) }

-- | Bind operation for Result
bindResult :: (a -> Result b) -> Result a -> Result b
bindResult f r = case resultValue r of
  Left err -> Result (Left err) (resultWarnings r)
  Right a -> let r' = f a
             in r' { resultWarnings = resultWarnings r ++ resultWarnings r' }

instance Functor Result where
  fmap = mapResult

instance Applicative Result where
  pure = success
  rf <*> ra = case (resultValue rf, resultValue ra) of
    (Left err, _) -> Result (Left err) (resultWarnings rf)
    (_, Left err) -> Result (Left err) (resultWarnings rf ++ resultWarnings ra)
    (Right f, Right a) -> Result (Right (f a)) (resultWarnings rf ++ resultWarnings ra)

instance Monad Result where
  (>>=) = flip bindResult

-- | Format an error for display
formatError :: AppError -> Text
formatError err = "[" <> errorCode err <> "] " <> errorMessage err

-- | Format a warning for display
formatWarning :: AppWarning -> Text
formatWarning w = "[" <> warningCode w <> "] " <> warningMessage w

-- | Format a result for display
formatResult :: Show a => Result a -> Text
formatResult r = case resultValue r of
  Left err -> "Error: " <> formatError err <>
              if null (resultWarnings r)
                then ""
                else "\nWarnings:\n" <> T.intercalate "\n" (map formatWarning (resultWarnings r))
  Right val -> "Success: " <> T.pack (show val) <>
               if null (resultWarnings r)
                 then ""
                 else "\nWarnings:\n" <> T.intercalate "\n" (map formatWarning (resultWarnings r))
