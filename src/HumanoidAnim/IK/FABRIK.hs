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

    -- * Internal (exported for testing)
  , findChainToFixed
  , getRootPosition
  , getBoneLengthsForChain
  , solveChainWithLengths
  , forwardPassWithAnchor
  , backwardPassWithAnchor
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
    boneLengthMap = skeletonLengths skeleton

    solveOne (currentPose, totalIters, allConverged, maxErr) effector =
      let effectorBone = constraintBone effector
          targetPos = constraintPosition effector

          -- Find nearest fixed bone that is ancestor of effector
          chain = findChainToFixed skeleton effectorBone fixedCs

          -- Get root position (from fixed constraint or current pose)
          rootPos = getRootPosition currentPose fixedCs chain

          -- Get bone lengths from skeleton definition (not from positions!)
          chainLengths = getBoneLengthsForChain boneLengthMap chain

          -- Solve this chain with correct bone lengths
          (newPositions, iters, conv, err) =
            solveChainWithLengths cfg chain currentPose chainLengths rootPos targetPos

          -- Merge results
          mergedPose = Map.union newPositions currentPose
      in (mergedPose, totalIters + iters, allConverged && conv, max maxErr err)

-- | Get bone lengths for a chain from the skeleton's bone length map
-- Returns lengths between consecutive bones in the chain
-- For a chain [A, B, C] with root R, we need:
--   [R→A distance, A→B distance, B→C distance]
-- The first length is from root (parent of A) to first chain bone (A)
-- Subsequent lengths are between consecutive chain bones
getBoneLengthsForChain :: Map HumanoidBone Float -> [HumanoidBone] -> [Float]
getBoneLengthsForChain boneLengthMap chain =
  -- For each bone in the chain, get its length (distance from its parent)
  -- bone length X = distance from parent(X) to X
  -- For chain [A, B, C], we get [length_A, length_B, length_C]
  -- which represents [R→A, A→B, B→C] where R is the root (parent of A)
  [fromMaybe 0.1 (Map.lookup bone boneLengthMap) | bone <- chain]

-- | Find the chain from effector to nearest appropriate fixed point
-- For arm chains, we use the shoulder as root (not the entire spine)
-- For leg chains, we use the upper leg as root
findChainToFixed
  :: Skeleton
  -> HumanoidBone
  -> [IKConstraint]
  -> [HumanoidBone]
findChainToFixed _skeleton effector fixedCs =
  let fixedBones = Set.fromList [constraintBone c | c <- fixedCs]
      ancestors = getBoneChain effector

      -- Determine the natural chain root based on limb type
      -- For arms: start from UpperArm (shoulder is considered fixed)
      -- For legs: start from UpperLeg (hip is considered fixed)
      naturalRoot = case effector of
        -- Right arm chain
        RightHand     -> Just RightUpperArm
        RightLowerArm -> Just RightUpperArm
        -- Left arm chain
        LeftHand      -> Just LeftUpperArm
        LeftLowerArm  -> Just LeftUpperArm
        -- Right leg chain
        RightFoot     -> Just RightUpperLeg
        RightToes     -> Just RightUpperLeg
        RightLowerLeg -> Just RightUpperLeg
        -- Left leg chain
        LeftFoot      -> Just LeftUpperLeg
        LeftToes      -> Just LeftUpperLeg
        LeftLowerLeg  -> Just LeftUpperLeg
        -- Other bones: use fixed constraints
        _ -> Nothing

      -- Choose chain root: natural root for limbs, or nearest fixed ancestor
      chainRoot = case naturalRoot of
        Just root -> root
        Nothing -> case filter (`Set.member` fixedBones) ancestors of
          (fixed:_) -> fixed
          [] -> Hips  -- Default to root if no fixed bone found

  in case getChainBetween chainRoot effector of
       -- getChainBetween doesn't include the 'from' bone, so prepend it
       Just chain -> chainRoot : chain
       Nothing -> [effector]  -- Fallback

-- | Get root position for a chain
-- For limb chains, use the parent bone's position (e.g., Shoulder for arm chains)
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
        [] ->
          -- For limb chains, get position from parent (shoulder/hip)
          let parentBone = case root of
                RightUpperArm -> Just RightShoulder
                LeftUpperArm  -> Just LeftShoulder
                RightUpperLeg -> Nothing  -- Hip position from Hips
                LeftUpperLeg  -> Nothing
                _ -> Nothing
              parentPos = case parentBone of
                Just p  -> Map.lookup p pose
                Nothing -> Nothing
          in case parentPos of
               Just pos -> pos
               Nothing  -> fromMaybe (V3 0 0 0) $ Map.lookup root pose
    [] -> V3 0 0 0

-- | Solve a single IK chain using FABRIK (legacy version, computes lengths from positions)
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
  | otherwise =
      let initialPositions = [fromMaybe (V3 0 0 0) (Map.lookup b pose) | b <- chain]
          -- Compute bone lengths from positions (legacy behavior)
          boneLengths = zipWith distance initialPositions (tail initialPositions)
      in solveChainWithLengths cfg chain pose boneLengths rootPos targetPos

-- | Solve a single IK chain using FABRIK with explicit bone lengths
-- The anchor position (rootPos) is the parent of the first bone in the chain.
-- All bones in the chain can move, but the first bone is constrained to be
-- at distance boneLengths[0] from the anchor.
--
-- For a chain [A, B, C] with boneLengths [lenA, lenB, lenC]:
--   lenA = distance from anchor to A
--   lenB = distance from A to B
--   lenC = distance from B to C
solveChainWithLengths
  :: FABRIKConfig
  -> [HumanoidBone]         -- ^ Chain from root to effector
  -> Map HumanoidBone (V3 Float)  -- ^ Current positions
  -> [Float]                -- ^ Bone lengths (from skeleton definition)
  -> V3 Float               -- ^ Anchor position (parent of first bone, fixed)
  -> V3 Float               -- ^ Target position for effector
  -> (Map HumanoidBone (V3 Float), Int, Bool, Float)
solveChainWithLengths cfg chain pose boneLengths anchorPos targetPos
  | null chain = (Map.empty, 0, True, 0)
  | null boneLengths = (Map.empty, 0, True, 0)
  | length chain == 1 =
      -- Single bone: place it at distance boneLengths[0] from anchor toward target
      let len = head boneLengths
          dir = if distance anchorPos targetPos < 0.0001
                then V3 1 0 0
                else normalize (targetPos ^-^ anchorPos)
          pos = anchorPos ^+^ (dir ^* len)
          err = distance pos targetPos
      in (Map.singleton (head chain) pos, 1, err < fabrikTolerance cfg, err)
  | otherwise = iterate' 0 initialPositions
  where
    -- Extract initial positions for chain
    initialPositions = [fromMaybe (V3 0 0 0) (Map.lookup b pose) | b <- chain]

    -- Total chain length
    totalLength = sum boneLengths

    -- Distance from anchor to target
    targetDist = distance anchorPos targetPos

    -- Check if target is reachable
    _isReachable = targetDist <= totalLength * 1.01  -- 1% tolerance

    -- Iterate until convergence
    iterate' iter positions
      | iter >= fabrikMaxIterations cfg =
          let err = distance (last positions) targetPos
          in (makeMap positions, iter, False, err)
      | err < fabrikTolerance cfg =
          (makeMap positions, iter, True, err)
      | otherwise =
          let -- Forward pass: move effector to target, propagate back
              afterForward = forwardPassWithAnchor targetPos boneLengths positions
              -- Backward pass: anchor first bone, propagate forward
              afterBackward = backwardPassWithAnchor anchorPos boneLengths afterForward
          in iterate' (iter + 1) afterBackward
      where
        err = distance (last positions) targetPos

    -- Convert positions list back to map
    makeMap positions = Map.fromList $ zip chain positions

-- | Forward pass: move effector to target, then propagate to root
-- For N bones with N lengths [L0, L1, ..., L(N-1)]:
--   L0 = anchor to bone[0] distance (used in backward pass only)
--   L1 = bone[0] to bone[1] distance
--   ...
--   L(N-1) = bone[N-2] to bone[N-1] distance
-- In forward pass: set bone[N-1] = target, then for i = N-2 down to 0:
--   bone[i] = bone[i+1] + dir * L(i+1)
forwardPassWithAnchor :: V3 Float -> [Float] -> [V3 Float] -> [V3 Float]
forwardPassWithAnchor target lengths positions
  | null positions = []
  | length positions == 1 = [target]  -- Single bone: just place at target
  | otherwise =
      -- Work backward from target
      -- Pair each position (except last) with the length to its child
      -- lengths[1:] are the inter-bone distances
      let n = length positions
          interLengths = tail lengths  -- [L1, L2, ..., L(N-1)]
          -- Build from effector backward
          -- positions[N-1] = target
          -- positions[i] = positions[i+1] + direction * lengths[i+1]
          result = foldr step [target] (zip (init positions) interLengths)
      in result
  where
    step (oldPos, len) newPositions =
      let nextPos = head newPositions  -- The child bone's new position
          dir = if norm (oldPos ^-^ nextPos) < 0.0001
                then V3 0 1 0
                else normalize (oldPos ^-^ nextPos)
          newPos = nextPos ^+^ (dir ^* len)
      in newPos : newPositions

-- | Backward pass: anchor first bone, then propagate to effector
-- Uses lengths[0] for anchor-to-bone[0], lengths[1:] for inter-bone
backwardPassWithAnchor :: V3 Float -> [Float] -> [V3 Float] -> [V3 Float]
backwardPassWithAnchor anchor lengths positions
  | null positions = []
  | null lengths = positions
  | otherwise =
      -- First bone is at distance lengths[0] from anchor toward its old position
      let firstLen = head lengths
          firstOldPos = head positions
          dir = if norm (firstOldPos ^-^ anchor) < 0.0001
                then V3 1 0 0
                else normalize (firstOldPos ^-^ anchor)
          firstNewPos = anchor ^+^ (dir ^* firstLen)
          -- Propagate forward using inter-bone lengths
          interLengths = tail lengths
          restOldPositions = tail positions
          newPositions = foldl' step [firstNewPos] (zip restOldPositions interLengths)
      in newPositions
  where
    step ps@(p:_) (oldPos, len) =
      let dir = if norm (oldPos ^-^ p) < 0.0001
                then V3 0 1 0
                else normalize (oldPos ^-^ p)
          newPos = p ^+^ (dir ^* len)
      in ps ++ [newPos]
    step [] _ = []

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
