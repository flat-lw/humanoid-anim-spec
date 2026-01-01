-- |
-- Module      : HumanoidAnim.IK.FABRIK
-- Description : FABRIK (Forward And Backward Reaching IK) solver
--
-- Implementation of the FABRIK algorithm for inverse kinematics.
-- FABRIK is an iterative solver that alternates between forward and
-- backward reaching passes to converge on a solution.
module HumanoidAnim.IK.FABRIK
  ( -- * Solver Type
    FABRIK(..)
  , FABRIKConfig(..)

    -- * Solving Functions
  , solveFABRIK
  , defaultFABRIKConfig
  , solveChain
  , forwardPass
  , backwardPass
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Linear (V3(..), norm, normalize, (^*), (^-^), (^+^))

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Hierarchy
import HumanoidAnim.Skeleton.Config (Skeleton(..))
import HumanoidAnim.IK.Core

-- | FABRIK solver singleton type
data FABRIK = FABRIK
  deriving stock (Show, Eq)

-- | FABRIK configuration
data FABRIKConfig = FABRIKConfig
  { fabrikMaxIterations :: Int
    -- ^ Maximum iterations before giving up (default: 50)
  , fabrikTolerance :: Float
    -- ^ Convergence tolerance in meters (default: 0.001)
  } deriving stock (Show, Eq)

-- | Default FABRIK configuration
defaultFABRIKConfig :: FABRIKConfig
defaultFABRIKConfig = FABRIKConfig
  { fabrikMaxIterations = 50
  , fabrikTolerance = 0.001
  }

-- | Main FABRIK solver entry point
solveFABRIK :: FABRIKConfig -> IKInput -> IKOutput
solveFABRIK cfg input = IKOutput
  { ikResultPose = finalPose
  , ikIterations = iterations
  , ikConverged = converged
  , ikError = finalError
  , ikWarnings = warnings
  }
  where
    skeleton = ikSkeleton input
    initialPose = ikInitialPose input
    constraints = ikConstraints input

    -- Separate fixed constraints and effector constraints
    fixedConstraints = [c | c@(Fixed _ _) <- constraints]
    effectorConstraints = [c | c@(Effector _ _) <- constraints] ++
                          [Effector b p | EffectorWithRotation b p _ <- constraints]

    -- For each effector, find the chain to nearest fixed bone and solve
    (finalPose, iterations, converged, finalError) =
      solveMultipleChains cfg skeleton initialPose fixedConstraints effectorConstraints

    -- Generate warnings
    warnings = generateWarnings cfg converged finalError

-- | Solve multiple IK chains
solveMultipleChains
  :: FABRIKConfig
  -> Skeleton
  -> Map HumanoidBone (V3 Float)
  -> [IKConstraint]  -- ^ Fixed constraints
  -> [IKConstraint]  -- ^ Effector constraints
  -> (Map HumanoidBone (V3 Float), Int, Bool, Float)
solveMultipleChains cfg skeleton pose fixedCs effectorCs =
  case effectorCs of
    [] -> (pose, 0, True, 0)
    _ -> foldl' solveOne (pose, 0, True, 0) effectorCs
  where
    solveOne (currentPose, totalIters, allConverged, maxErr) effector =
      let effectorBone = constraintBone effector
          targetPos = constraintPosition effector

          -- Find nearest fixed bone that is ancestor of effector
          chain = findChainToFixed skeleton effectorBone fixedCs

          -- Get root position (from fixed constraint or current pose)
          rootPos = getRootPosition currentPose fixedCs chain

          -- Solve this chain
          (newPositions, iters, conv, err) =
            solveChain cfg chain currentPose rootPos targetPos

          -- Merge results
          mergedPose = Map.union newPositions currentPose
      in (mergedPose, totalIters + iters, allConverged && conv, max maxErr err)

-- | Find the chain from effector to nearest fixed ancestor
findChainToFixed
  :: Skeleton
  -> HumanoidBone
  -> [IKConstraint]
  -> [HumanoidBone]
findChainToFixed _skeleton effector fixedCs =
  let fixedBones = Set.fromList [constraintBone c | c <- fixedCs]
      ancestors = getBoneChain effector
      -- Find first fixed ancestor
      chainEnd = case filter (`Set.member` fixedBones) ancestors of
        (fixed:_) -> fixed
        [] -> Hips  -- Default to root if no fixed bone found
  in case getChainBetween chainEnd effector of
       Just chain -> chain
       Nothing -> [effector]  -- Fallback

-- | Get root position for a chain
getRootPosition
  :: Map HumanoidBone (V3 Float)
  -> [IKConstraint]
  -> [HumanoidBone]
  -> V3 Float
getRootPosition pose fixedCs chain =
  case chain of
    (root:_) ->
      -- Check if root has a fixed constraint
      case [constraintPosition c | c <- fixedCs, constraintBone c == root] of
        (pos:_) -> pos
        [] -> fromMaybe (V3 0 0 0) $ Map.lookup root pose
    [] -> V3 0 0 0

-- | Solve a single IK chain using FABRIK
solveChain
  :: FABRIKConfig
  -> [HumanoidBone]         -- ^ Chain from root to effector
  -> Map HumanoidBone (V3 Float)  -- ^ Current positions
  -> V3 Float               -- ^ Root position (fixed)
  -> V3 Float               -- ^ Target position for effector
  -> (Map HumanoidBone (V3 Float), Int, Bool, Float)
solveChain cfg chain pose rootPos targetPos
  | null chain = (Map.empty, 0, True, 0)
  | length chain == 1 = (Map.singleton (head chain) targetPos, 0, True, 0)
  | otherwise = iterate' 0 initialPositions
  where
    -- Extract initial positions for chain
    initialPositions = [fromMaybe (V3 0 0 0) (Map.lookup b pose) | b <- chain]

    -- Compute bone lengths from positions
    boneLengths = zipWith distance initialPositions (tail initialPositions)

    -- Iterate until convergence
    iterate' iter positions
      | iter >= fabrikMaxIterations cfg =
          let err = distance (last positions) targetPos
          in (makeMap positions, iter, False, err)
      | err < fabrikTolerance cfg =
          (makeMap positions, iter, True, err)
      | otherwise =
          let -- Forward pass: from effector to root
              afterForward = forwardPass targetPos boneLengths positions
              -- Backward pass: from root to effector
              afterBackward = backwardPass rootPos boneLengths afterForward
          in iterate' (iter + 1) afterBackward
      where
        err = distance (last positions) targetPos

    -- Convert positions list back to map
    makeMap positions = Map.fromList $ zip chain positions

-- | Forward pass: move from effector toward target, then propagate to root
forwardPass :: V3 Float -> [Float] -> [V3 Float] -> [V3 Float]
forwardPass target lengths positions
  | null positions = []
  | null lengths = positions
  | otherwise = reverse $ foldl' step [target] (zip (reverse $ init positions) (reverse lengths))
  where
    step [] _ = []
    step (p:ps) (_, len) =
      let prevPos = case ps of
            [] -> head positions
            (prev:_) -> prev
          dir = if norm (prevPos ^-^ p) < 0.0001
                then V3 0 1 0
                else normalize (prevPos ^-^ p)
          newPos = p ^+^ (dir ^* len)
      in newPos : p : ps

-- | Backward pass: fix root position, then propagate to effector
backwardPass :: V3 Float -> [Float] -> [V3 Float] -> [V3 Float]
backwardPass root lengths positions
  | null positions = []
  | null lengths = [root]
  | otherwise = foldl' step [root] (zip (tail positions) lengths)
  where
    step [] _ = []
    step ps@(p:_) (nextOld, len) =
      let dir = if norm (nextOld ^-^ p) < 0.0001
                then V3 0 1 0
                else normalize (nextOld ^-^ p)
          newPos = p ^+^ (dir ^* len)
      in ps ++ [newPos]

-- | Euclidean distance between two points
distance :: V3 Float -> V3 Float -> Float
distance a b = norm (a ^-^ b)

-- | Generate warnings based on solve results
generateWarnings
  :: FABRIKConfig
  -> Bool
  -> Float
  -> [IKWarning]
generateWarnings cfg converged finalError =
  if converged
    then []
    else [DidNotConverge (fabrikMaxIterations cfg) finalError]
