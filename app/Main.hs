{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Main
-- Description : CLI entry point for humanoid-anim
--
-- Command-line interface for the humanoid animation generator.
module Main (main) where

import Control.Monad (when, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)

import HumanoidAnim

-- | CLI command types
data Command
  = Generate GenerateOpts
  | Validate ValidateOpts
  | Info InfoOpts
  deriving stock (Show)

-- | Options for generate command
data GenerateOpts = GenerateOpts
  { genInput :: FilePath
  , genOutput :: FilePath
  , genFormat :: Maybe String
  , genSolver :: Maybe String
  , genFps :: Maybe Float
  , genFrames :: Maybe Int
  , genLoop :: Maybe String
  , genNoOptimize :: Bool
  , genVerbose :: Bool
  , genQuiet :: Bool
  , genStrict :: Bool
  } deriving stock (Show)

-- | Options for validate command
data ValidateOpts = ValidateOpts
  { valInput :: FilePath
  , valStrict :: Bool
  } deriving stock (Show)

-- | Options for info command
data InfoOpts = InfoOpts
  { infoSkeleton :: Maybe String
  , infoBones :: Bool
  , infoHierarchy :: Bool
  } deriving stock (Show)

-- | Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Generate genOpts -> runGenerate genOpts
    Validate valOpts -> runValidate valOpts
    Info infoOpts -> runInfo infoOpts
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Generate animation clips for Unity Humanoid rig"
     <> header "humanoid-anim - Humanoid Animation Generator"
      )

-- | Parse commands
commandParser :: Parser Command
commandParser = subparser
  ( command "generate"
      (info (Generate <$> generateOptsParser)
            (progDesc "Generate animation from configuration"))
 <> command "validate"
      (info (Validate <$> validateOptsParser)
            (progDesc "Validate configuration file"))
 <> command "info"
      (info (Info <$> infoOptsParser)
            (progDesc "Show skeleton information"))
  )

-- | Parse generate options
generateOptsParser :: Parser GenerateOpts
generateOptsParser = GenerateOpts
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Input configuration file (.yaml or .json)"
      )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file (.glb or .gltf)"
      )
  <*> optional (strOption
      ( long "format"
     <> short 'f'
     <> metavar "FORMAT"
     <> help "Output format: glb | gltf (default: glb)"
      ))
  <*> optional (strOption
      ( long "solver"
     <> metavar "SOLVER"
     <> help "IK solver: fabrik | ccd (default: fabrik)"
      ))
  <*> optional (option auto
      ( long "fps"
     <> metavar "N"
     <> help "Frame rate (default: 30)"
      ))
  <*> optional (option auto
      ( long "frames"
     <> metavar "N"
     <> help "Number of frames (overrides fps)"
      ))
  <*> optional (strOption
      ( long "loop"
     <> metavar "MODE"
     <> help "Loop mode: once | cycle | pingpong (default: once)"
      ))
  <*> switch
      ( long "no-optimize"
     <> help "Disable keyframe optimization"
      )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output"
      )
  <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "Quiet mode (errors only)"
      )
  <*> switch
      ( long "strict"
     <> help "Treat warnings as errors"
      )

-- | Parse validate options
validateOptsParser :: Parser ValidateOpts
validateOptsParser = ValidateOpts
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Configuration file to validate"
      )
  <*> switch
      ( long "strict"
     <> help "Strict mode (show all warnings)"
      )

-- | Parse info options
infoOptsParser :: Parser InfoOpts
infoOptsParser = InfoOpts
  <$> optional (strOption
      ( long "skeleton"
     <> metavar "DETAIL"
     <> help "Skeleton detail: minimal | standard | full"
      ))
  <*> switch
      ( long "bones"
     <> help "Show bone list"
      )
  <*> switch
      ( long "hierarchy"
     <> help "Show bone hierarchy"
      )

-- | Run generate command
runGenerate :: GenerateOpts -> IO ()
runGenerate opts = do
  let inputPath = genInput opts
      outputPath = genOutput opts
      verbose = genVerbose opts
      quiet = genQuiet opts
      strict = genStrict opts

  when verbose $ putStrLn $ "Loading configuration from: " ++ inputPath

  -- Load and generate
  result <- generateFromFile inputPath

  case resultValue result of
    Left err -> do
      TIO.putStrLn $ "Error: " <> formatError err
      exitFailure

    Right clip -> do
      -- Check warnings
      let warnings = resultWarnings result
      when (strict && not (null warnings)) $ do
        TIO.putStrLn "Strict mode: treating warnings as errors"
        forM_ warnings $ \w -> TIO.putStrLn $ formatWarning w
        exitFailure

      -- Show warnings unless quiet
      when (not quiet && not (null warnings)) $ do
        TIO.putStrLn "Warnings:"
        forM_ warnings $ \w -> TIO.putStrLn $ "  " <> formatWarning w

      -- Determine output format
      let format = case genFormat opts of
            Just "gltf" -> GLTF
            Just "glb" -> GLB
            Nothing -> case takeExtension outputPath of
              ".gltf" -> GLTF
              _ -> GLB
            _ -> GLB

      let gltfOpts = GLTFOptions
            { gltfFormat = format
            , gltfPrecision = 6
            , gltfOptimize = not (genNoOptimize opts)
            }

      -- Build skeleton for export
      let skeleton = buildSkeleton defaultSkeletonConfig

      -- Write output
      when verbose $ putStrLn $ "Writing output to: " ++ outputPath
      writeResult <- writeGLTF outputPath gltfOpts clip skeleton

      case resultValue writeResult of
        Left err -> do
          TIO.putStrLn $ "Error: " <> formatError err
          exitFailure
        Right () -> do
          when (not quiet) $ putStrLn $ "Successfully generated: " ++ outputPath
          exitSuccess

-- | Run validate command
runValidate :: ValidateOpts -> IO ()
runValidate opts = do
  let inputPath = valInput opts

  putStrLn $ "Validating: " ++ inputPath

  configResult <- loadConfigFromFile inputPath

  case resultValue configResult of
    Left err -> do
      TIO.putStrLn $ "Error: " <> formatError err
      exitFailure

    Right config -> do
      let validationResult = validateConfig config
      case resultValue validationResult of
        Left err -> do
          TIO.putStrLn $ "Validation failed: " <> formatError err
          exitFailure

        Right _ -> do
          let warnings = resultWarnings validationResult
          if null warnings
            then putStrLn "Configuration is valid."
            else do
              putStrLn "Configuration is valid with warnings:"
              forM_ warnings $ \w -> TIO.putStrLn $ "  " <> formatWarning w

          when (valStrict opts && not (null warnings)) $ do
            putStrLn "Strict mode: treating warnings as errors"
            exitFailure

          exitSuccess

-- | Run info command
runInfo :: InfoOpts -> IO ()
runInfo opts = do
  let detail = case infoSkeleton opts of
        Just "minimal" -> MinimalSkeleton
        Just "full" -> FullSkeleton
        _ -> StandardSkeleton

  when (infoBones opts || not (infoHierarchy opts)) $ do
    putStrLn $ "Skeleton detail: " ++ show detail
    putStrLn ""
    putStrLn "Bones:"
    let bones = bonesForDetail detail
    forM_ bones $ \bone -> do
      putStrLn $ "  " ++ show bone ++ " (" ++ show (boneCategory bone) ++ ")"

  when (infoHierarchy opts) $ do
    putStrLn ""
    putStrLn "Hierarchy:"
    printHierarchy 0 Hips

-- | Print bone hierarchy with indentation
printHierarchy :: Int -> HumanoidBone -> IO ()
printHierarchy indent bone = do
  let prefix = replicate (indent * 2) ' '
  putStrLn $ prefix ++ show bone
  let children = [b | b <- bonesForDetail StandardSkeleton
                    , boneParent b == Just bone]
  forM_ children $ printHierarchy (indent + 1)
