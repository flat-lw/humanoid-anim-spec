-- |
-- Module      : HumanoidAnim.Output.UnityAnim
-- Description : Unity .anim file format export
--
-- This module exports animation clips to Unity's native .anim format,
-- which is a YAML-based serialization format used by Unity's animation system.
-- Supports both Generic (Transform-based) and Humanoid (Muscle-based) export.
module HumanoidAnim.Output.UnityAnim
  ( -- * Export Functions
    exportUnityAnim
  , writeUnityAnim
  , exportUnityAnimHumanoid
  , writeUnityAnimHumanoid

    -- * Options
  , UnityAnimOptions(..)
  , WrapMode(..)
  , AnimationType(..)
  , defaultUnityAnimOptions

    -- * Unity Types
  , UnityAnimationClip(..)
  , UnityCurve(..)
  , UnityKeyframe(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Linear (V3(..), Quaternion(..), (^*), (^+^), (^/))
import Text.Printf (printf)

import HumanoidAnim.Animation
import HumanoidAnim.Output.Muscle
import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config (Transform(..))

-- | Animation type for export
data AnimationType
  = GenericAnimation   -- ^ Transform-based (position/rotation curves)
  | HumanoidAnimation  -- ^ Muscle-based (Unity Humanoid)
  deriving stock (Show, Eq, Enum, Bounded)

-- | Unity .anim export options
data UnityAnimOptions = UnityAnimOptions
  { uaoPrecision :: Int
    -- ^ Decimal precision for float values (default: 6)
  , uaoSampleRate :: Float
    -- ^ Sample rate in Hz (default: 60)
  , uaoWrapMode :: WrapMode
    -- ^ Animation wrap mode
  , uaoRootBonePath :: Text
    -- ^ Path prefix for root bone (default: "")
  , uaoAnimationType :: AnimationType
    -- ^ Animation type (Generic or Humanoid)
  } deriving stock (Show, Eq)

-- | Unity wrap mode
data WrapMode
  = WrapOnce
  | WrapLoop
  | WrapPingPong
  | WrapClampForever
  deriving stock (Show, Eq, Enum, Bounded)

-- | Default Unity animation options
defaultUnityAnimOptions :: UnityAnimOptions
defaultUnityAnimOptions = UnityAnimOptions
  { uaoPrecision = 6
  , uaoSampleRate = 60
  , uaoWrapMode = WrapOnce
  , uaoRootBonePath = ""
  , uaoAnimationType = HumanoidAnimation  -- Default to Humanoid
  }

-- | Unity animation clip structure
data UnityAnimationClip = UnityAnimationClip
  { uacName :: String
  , uacSampleRate :: Float
  , uacWrapMode :: WrapMode
  , uacPositionCurves :: [UnityCurve]
  , uacRotationCurves :: [UnityCurve]
  , uacScaleCurves :: [UnityCurve]
  , uacMuscleCurves :: [UnityCurve]  -- Humanoid muscle curves
  , uacRootMotionCurves :: [UnityCurve]  -- Root motion (RootT, RootQ)
  } deriving stock (Show, Eq)

-- | Unity animation curve
data UnityCurve = UnityCurve
  { ucPath :: String
    -- ^ Bone path in hierarchy (empty for muscle curves)
  , ucPropertyName :: String
    -- ^ Property name (e.g., "m_LocalPosition.x" or muscle name)
  , ucKeyframes :: [UnityKeyframe]
  } deriving stock (Show, Eq)

-- | Unity keyframe
data UnityKeyframe = UnityKeyframe
  { ukTime :: Float
  , ukValue :: Float
  , ukInTangent :: Float
  , ukOutTangent :: Float
  , ukInWeight :: Float
  , ukOutWeight :: Float
  , ukWeightedMode :: Int
  } deriving stock (Show, Eq)

-- | Export animation clip to Unity .anim format (auto-selects based on options)
exportUnityAnim :: UnityAnimOptions -> AnimationClip -> ByteString
exportUnityAnim opts clip = case uaoAnimationType opts of
  GenericAnimation -> exportUnityAnimGeneric opts clip
  HumanoidAnimation -> exportUnityAnimHumanoid opts clip

-- | Write Unity .anim file
writeUnityAnim :: FilePath -> UnityAnimOptions -> AnimationClip -> IO ()
writeUnityAnim path opts clip = BS.writeFile path (exportUnityAnim opts clip)

-- | Export animation clip to Unity .anim format (Generic/Transform-based)
exportUnityAnimGeneric :: UnityAnimOptions -> AnimationClip -> ByteString
exportUnityAnimGeneric opts clip =
  let unityClip = animToUnityGeneric opts clip
  in serializeUnityClip opts unityClip

-- | Export animation clip to Unity .anim format (Humanoid/Muscle-based)
exportUnityAnimHumanoid :: UnityAnimOptions -> AnimationClip -> ByteString
exportUnityAnimHumanoid opts clip =
  let unityClip = animToUnityHumanoid opts clip
  in serializeUnityClipHumanoid opts unityClip

-- | Write Unity .anim file (Humanoid)
writeUnityAnimHumanoid :: FilePath -> UnityAnimOptions -> AnimationClip -> IO ()
writeUnityAnimHumanoid path opts clip = BS.writeFile path (exportUnityAnimHumanoid opts clip)

-- | Convert AnimationClip to Unity format (Generic)
animToUnityGeneric :: UnityAnimOptions -> AnimationClip -> UnityAnimationClip
animToUnityGeneric opts clip =
  let frames = clipFrames clip
      bones = allBonesFromFrames frames
      posCurves = concatMap (makePosisionCurves opts frames) bones
      rotCurves = concatMap (makeRotationCurves opts frames) bones
      wrapMode = loopModeToWrap (clipLoopMode clip)
  in UnityAnimationClip
       { uacName = clipName clip
       , uacSampleRate = uaoSampleRate opts
       , uacWrapMode = wrapMode
       , uacPositionCurves = posCurves
       , uacRotationCurves = rotCurves
       , uacScaleCurves = []
       , uacMuscleCurves = []
       , uacRootMotionCurves = []
       }

-- | Convert AnimationClip to Unity format (Humanoid)
animToUnityHumanoid :: UnityAnimOptions -> AnimationClip -> UnityAnimationClip
animToUnityHumanoid opts clip =
  let frames = clipFrames clip
      wrapMode = loopModeToWrap (clipLoopMode clip)
      fixedBones = clipFixedBones clip

      -- Generate muscle curves from bone rotations
      muscleCurves = generateMuscleCurves frames

      -- Generate root motion curves with proper offsets for fixed bones
      rootMotionCurves = generateRootMotionCurves frames fixedBones

  in UnityAnimationClip
       { uacName = clipName clip
       , uacSampleRate = uaoSampleRate opts
       , uacWrapMode = wrapMode
       , uacPositionCurves = []
       , uacRotationCurves = []
       , uacScaleCurves = []
       , uacMuscleCurves = muscleCurves
       , uacRootMotionCurves = rootMotionCurves
       }

-- | Generate muscle curves from animation frames
generateMuscleCurves :: [AnimationFrame] -> [UnityCurve]
generateMuscleCurves frames =
  let -- Get all bones from first frame
      bones = case frames of
        (f:_) -> Map.keys (framePose f)
        [] -> []

      -- Get all muscles for these bones
      allMuscleIds = nub $ concatMap boneToMuscles bones

      -- Generate a curve for each muscle
      curves = map (generateMuscleCurve frames bones) allMuscleIds

  in curves

-- | Generate a single muscle curve
generateMuscleCurve :: [AnimationFrame] -> [HumanoidBone] -> MuscleId -> UnityCurve
generateMuscleCurve frames bones muscleId =
  let propName = musclePropertyName muscleId

      -- Find which bone this muscle belongs to
      boneMuscles = [(bone, boneToMuscles bone) | bone <- bones]
      matchingBone = [bone | (bone, muscles) <- boneMuscles, muscleId `elem` muscles]

      -- Generate keyframes
      keyframes = map (generateMuscleKeyframe matchingBone muscleId) frames

  in UnityCurve "" propName keyframes

-- | Generate a muscle keyframe
generateMuscleKeyframe :: [HumanoidBone] -> MuscleId -> AnimationFrame -> UnityKeyframe
generateMuscleKeyframe matchingBones muscleId frame =
  let muscleValue = case matchingBones of
        (bone:_) ->
          case Map.lookup bone (framePose frame) of
            Just transform ->
              -- For limb bones, use position-based calculation
              if bone `elem` [RightUpperArm, LeftUpperArm, RightUpperLeg, LeftUpperLeg,
                              RightLowerArm, LeftLowerArm, RightLowerLeg, LeftLowerLeg]
              then computeLimbMuscle bone muscleId transform (framePose frame)
              else
                -- For other bones, use quaternion-based calculation
                let quat = transformRotation transform
                    muscles = quaternionToMuscles bone quat
                in case lookup muscleId muscles of
                     Just v -> v
                     Nothing -> 0
            Nothing -> 0
        [] -> 0
  in makeKeyframe (frameTime frame) muscleValue

-- | Compute muscle value for limb bones using positions
computeLimbMuscle :: HumanoidBone -> MuscleId -> Transform -> Map.Map HumanoidBone Transform -> Float
computeLimbMuscle bone muscleId transform pose =
  case bone of
    -- Upper limbs use parent-to-child direction
    RightUpperArm -> computeUpperLimbMuscle bone muscleId transform pose
    LeftUpperArm  -> computeUpperLimbMuscle bone muscleId transform pose
    RightUpperLeg -> computeUpperLimbMuscle bone muscleId transform pose
    LeftUpperLeg  -> computeUpperLimbMuscle bone muscleId transform pose
    -- Lower limbs use knee/elbow angle
    RightLowerArm -> computeLowerLimbMuscle bone muscleId pose
    LeftLowerArm  -> computeLowerLimbMuscle bone muscleId pose
    RightLowerLeg -> computeLowerLimbMuscle bone muscleId pose
    LeftLowerLeg  -> computeLowerLimbMuscle bone muscleId pose
    _ -> 0

-- | Compute muscle for upper limb (UpperArm, UpperLeg)
-- Uses IK-computed positions from pose, not the fixed skeleton transform
computeUpperLimbMuscle :: HumanoidBone -> MuscleId -> Transform -> Map.Map HumanoidBone Transform -> Float
computeUpperLimbMuscle bone muscleId _transform pose =
  case (Map.lookup bone pose, getChildPosition bone pose) of
    (Just parentTransform, Just childPos) ->
      let parentPos = transformPosition parentTransform
          fetusDir = fetusDirection bone
          muscles = positionToMuscles bone parentPos childPos fetusDir
      in case lookup muscleId muscles of
           Just v -> v
           Nothing -> 0
    _ -> 0

-- | Compute muscle for lower limb (LowerArm, LowerLeg) - especially Stretch
--
-- Unity Humanoid Stretch muscle (based on unity_humanoid_center_of_mass.md):
--   Forearm/Lower Leg Stretch: min=-80, max=80, center=-80
--   muscle = 1: fully extended (straight, angle = 0°)
--   muscle = 0: center of range (angle = -80°, i.e., 80° flexion)
--   muscle = -1: fully bent (angle = -160°, i.e., 160° flexion)
--
-- The angle convention:
--   0° = straight (elbow/knee fully extended)
--   negative = flexion (bent)
--
-- Formula from documentation:
--   if angle >= center: muscle = (angle - center) / max
--   else:               muscle = (angle - center) / (-min)
computeLowerLimbMuscle :: HumanoidBone -> MuscleId -> Map.Map HumanoidBone Transform -> Float
computeLowerLimbMuscle bone muscleId pose =
  let -- Get the three bone positions for angle calculation
      (upperBone, endBone) = case bone of
        RightLowerArm -> (RightUpperArm, RightHand)
        LeftLowerArm  -> (LeftUpperArm, LeftHand)
        RightLowerLeg -> (RightUpperLeg, RightFoot)
        LeftLowerLeg  -> (LeftUpperLeg, LeftFoot)
        _ -> (bone, bone)

      mUpperPos = transformPosition <$> Map.lookup upperBone pose
      mMidPos = transformPosition <$> Map.lookup bone pose
      mEndPos = transformPosition <$> Map.lookup endBone pose

  in case (mUpperPos, mMidPos, mEndPos) of
       (Just upperPos, Just midPos, Just endPos) ->
         let -- Calculate angle at the mid joint (elbow/knee)
             v1 = upperPos - midPos  -- Vector from mid to upper
             v2 = endPos - midPos    -- Vector from mid to end

             -- Compute angle using dot product
             dotProd = v1 `dotV` v2
             len1 = normV v1
             len2 = normV v2
             cosAngle = if len1 > 0.0001 && len2 > 0.0001
                        then dotProd / (len1 * len2)
                        else 1
             angleRad = acos (max (-1) (min 1 cosAngle))
             measuredAngleDeg = angleRad * 180 / pi

             -- Convert measured angle (180° = straight, <180° = bent) to stretch angle
             -- Stretch angle: 0° = straight, negative = bent
             stretchAngle = measuredAngleDeg - 180

             -- Apply the documented formula
             -- center = -80, max = 80, min = -80
             stretchCenter = -80 :: Float
             stretchMax = 80 :: Float
             stretchMin = -80 :: Float

             stretchValue = if stretchAngle >= stretchCenter
                            then clamp (-1) 1 ((stretchAngle - stretchCenter) / stretchMax)
                            else clamp (-1) 1 ((stretchAngle - stretchCenter) / (-stretchMin))

         in case muscleId of
              RightLegStretch -> stretchValue
              LeftLegStretch -> stretchValue
              RightForearmStretch -> stretchValue
              LeftForearmStretch -> stretchValue
              -- For twist, fall back to quaternion
              _ -> let quat = maybe (Quaternion 1 (V3 0 0 0)) transformRotation (Map.lookup bone pose)
                       muscles = quaternionToMuscles bone quat
                   in case lookup muscleId muscles of
                        Just v -> v
                        Nothing -> 0
       _ -> 0
  where
    dotV (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
    normV (V3 x y z) = sqrt (x*x + y*y + z*z)
    clamp lo hi x = max lo (min hi x)

-- | Get child bone position for a given bone
getChildPosition :: HumanoidBone -> Map.Map HumanoidBone Transform -> Maybe (V3 Float)
getChildPosition bone pose =
  let childBone = case bone of
        RightUpperArm -> Just RightLowerArm
        LeftUpperArm  -> Just LeftLowerArm
        RightUpperLeg -> Just RightLowerLeg
        LeftUpperLeg  -> Just LeftLowerLeg
        _ -> Nothing
  in case childBone of
       Just child -> transformPosition <$> Map.lookup child pose
       Nothing -> Nothing

-- ============================================================================
-- Mass Ratios for Center of Mass Calculation
-- ============================================================================
--
-- Based on biomechanics data (de Leva 1996, Dempster 1955)
-- Used by Unity Mecanim for Humanoid retargeting

-- | Mass ratio for each bone (percentage of total body mass)
boneMassRatio :: HumanoidBone -> Float
boneMassRatio bone = case bone of
  Hips          -> 0.117   -- Lower trunk
  Spine         -> 0.084   -- Mid trunk (half)
  Chest         -> 0.084   -- Mid trunk (half)
  UpperChest    -> 0.081   -- Upper trunk
  Neck          -> 0.0405  -- Head+Neck (half)
  Head          -> 0.0405  -- Head+Neck (half)
  LeftUpperArm  -> 0.027   -- Upper arm
  LeftLowerArm  -> 0.016   -- Forearm
  LeftHand      -> 0.006   -- Hand
  RightUpperArm -> 0.027
  RightLowerArm -> 0.016
  RightHand     -> 0.006
  LeftUpperLeg  -> 0.100   -- Thigh
  LeftLowerLeg  -> 0.043   -- Shank
  LeftFoot      -> 0.014   -- Foot
  RightUpperLeg -> 0.100
  RightLowerLeg -> 0.043
  RightFoot     -> 0.014
  -- Bones without significant mass contribution
  _             -> 0.0

-- | List of bones used for center of mass calculation
massBonesForCoM :: [HumanoidBone]
massBonesForCoM =
  [ Hips, Spine, Chest, UpperChest, Neck, Head
  , LeftUpperArm, LeftLowerArm, LeftHand
  , RightUpperArm, RightLowerArm, RightHand
  , LeftUpperLeg, LeftLowerLeg, LeftFoot
  , RightUpperLeg, RightLowerLeg, RightFoot
  ]

-- | Calculate center of mass Y coordinate from bone positions
--
-- CoM.y = Σ(Bone.y × MassRatio) / Σ(MassRatio)
calculateCenterOfMassY :: Map.Map HumanoidBone Transform -> Float
calculateCenterOfMassY pose =
  let -- Get weighted Y for each bone
      weightedYs = [(y * boneMassRatio bone, boneMassRatio bone)
                   | bone <- massBonesForCoM
                   , Just t <- [Map.lookup bone pose]
                   , let V3 _ y _ = transformPosition t
                   ]
      totalWeightedY = sum (map fst weightedYs)
      totalMass = sum (map snd weightedYs)
  in if totalMass > 0
     then totalWeightedY / totalMass
     else 0

-- | Calculate humanScale from T-Pose (first frame)
--
-- humanScale = T-Pose時の質量中心のY座標
calculateHumanScale :: [AnimationFrame] -> Float
calculateHumanScale [] = 1.0  -- Default if no frames
calculateHumanScale (firstFrame:_) =
  let comY = calculateCenterOfMassY (framePose firstFrame)
  in if comY > 0 then comY else 1.0

-- ============================================================================
-- Root Motion Curves
-- ============================================================================

-- | Generate root motion curves (RootT.x, RootT.y, RootT.z, RootQ.x, RootQ.y, RootQ.z, RootQ.w)
--
-- RootT.x/z = Center of Mass x/z (normalized)
-- RootT.y = (Center of Mass Y - minY) / humanScale
-- RootQ = identity
generateRootMotionCurves :: [AnimationFrame] -> [(HumanoidBone, V3 Float)] -> [UnityCurve]
generateRootMotionCurves frames _fixedBones =
  let -- Calculate humanScale from first frame (T-Pose assumption)
      humanScale = calculateHumanScale frames

      -- Get center of mass for a frame
      getCenterOfMass :: AnimationFrame -> V3 Float
      getCenterOfMass frame =
        let pose = framePose frame
            weightedPositions = [(transformPosition t ^* boneMassRatio bone, boneMassRatio bone)
                                | bone <- massBonesForCoM
                                , Just t <- [Map.lookup bone pose]
                                ]
            totalWeighted = foldr (^+^) (V3 0 0 0) (map fst weightedPositions)
            totalMass = sum (map snd weightedPositions)
        in if totalMass > 0
           then totalWeighted ^/ totalMass
           else V3 0 0 0

      -- Get minimum Y across all bones in a frame
      getMinY :: AnimationFrame -> Float
      getMinY frame =
        let positions = map transformPosition (Map.elems (framePose frame))
        in if null positions
           then 0
           else minimum [y | V3 _ y _ <- positions]

      -- Compute RootT: normalized center of mass with Y offset
      -- RootT.y = (CoM.y - minY) / humanScale
      computeRootT :: AnimationFrame -> V3 Float
      computeRootT frame =
        let V3 comX comY comZ = getCenterOfMass frame
            minY = getMinY frame
            normalizedY = (comY - minY) / humanScale
        in V3 (comX / humanScale) normalizedY (comZ / humanScale)

      -- RootQ is identity
      identityQuat = Quaternion 1 (V3 0 0 0)

      -- Position curves
      txKeys = [makeKeyframe (frameTime f) ((\(V3 x _ _) -> x) (computeRootT f)) | f <- frames]
      tyKeys = [makeKeyframe (frameTime f) ((\(V3 _ y _) -> y) (computeRootT f)) | f <- frames]
      tzKeys = [makeKeyframe (frameTime f) ((\(V3 _ _ z) -> z) (computeRootT f)) | f <- frames]

      -- Rotation curves (identity quaternion)
      rxKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 x _ _)) -> x) identityQuat) | f <- frames]
      ryKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 _ y _)) -> y) identityQuat) | f <- frames]
      rzKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 _ _ z)) -> z) identityQuat) | f <- frames]
      rwKeys = [makeKeyframe (frameTime f) ((\(Quaternion w _) -> w) identityQuat) | f <- frames]

  in [ UnityCurve "" "RootT.x" txKeys
     , UnityCurve "" "RootT.y" tyKeys
     , UnityCurve "" "RootT.z" tzKeys
     , UnityCurve "" "RootQ.x" rxKeys
     , UnityCurve "" "RootQ.y" ryKeys
     , UnityCurve "" "RootQ.z" rzKeys
     , UnityCurve "" "RootQ.w" rwKeys
     ]

-- | Get all bones from frames
allBonesFromFrames :: [AnimationFrame] -> [HumanoidBone]
allBonesFromFrames [] = []
allBonesFromFrames (f:_) = Map.keys (framePose f)

-- | Convert loop mode to wrap mode
loopModeToWrap :: LoopMode -> WrapMode
loopModeToWrap Once = WrapOnce
loopModeToWrap Cycle = WrapLoop
loopModeToWrap PingPong = WrapPingPong

-- | Make position curves for a bone
makePosisionCurves :: UnityAnimOptions -> [AnimationFrame] -> HumanoidBone -> [UnityCurve]
makePosisionCurves opts frames bone =
  let path = boneToPath opts bone
      getPos frame = case Map.lookup bone (framePose frame) of
        Just t -> transformPosition t
        Nothing -> V3 0 0 0

      xKeys = [makeKeyframe (frameTime f) ((\(V3 x _ _) -> x) (getPos f)) | f <- frames]
      yKeys = [makeKeyframe (frameTime f) ((\(V3 _ y _) -> y) (getPos f)) | f <- frames]
      zKeys = [makeKeyframe (frameTime f) ((\(V3 _ _ z) -> z) (getPos f)) | f <- frames]
  in [ UnityCurve path "m_LocalPosition.x" xKeys
     , UnityCurve path "m_LocalPosition.y" yKeys
     , UnityCurve path "m_LocalPosition.z" zKeys
     ]

-- | Make rotation curves for a bone (quaternion)
makeRotationCurves :: UnityAnimOptions -> [AnimationFrame] -> HumanoidBone -> [UnityCurve]
makeRotationCurves opts frames bone =
  let path = boneToPath opts bone
      getRot frame = case Map.lookup bone (framePose frame) of
        Just t -> transformRotation t
        Nothing -> Quaternion 1 (V3 0 0 0)

      xKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 x _ _)) -> x) (getRot f)) | f <- frames]
      yKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 _ y _)) -> y) (getRot f)) | f <- frames]
      zKeys = [makeKeyframe (frameTime f) ((\(Quaternion _ (V3 _ _ z)) -> z) (getRot f)) | f <- frames]
      wKeys = [makeKeyframe (frameTime f) ((\(Quaternion w _) -> w) (getRot f)) | f <- frames]
  in [ UnityCurve path "m_LocalRotation.x" xKeys
     , UnityCurve path "m_LocalRotation.y" yKeys
     , UnityCurve path "m_LocalRotation.z" zKeys
     , UnityCurve path "m_LocalRotation.w" wKeys
     ]

-- | Make a keyframe with auto tangents
makeKeyframe :: Float -> Float -> UnityKeyframe
makeKeyframe time value = UnityKeyframe
  { ukTime = time
  , ukValue = value
  , ukInTangent = 0  -- Auto tangent
  , ukOutTangent = 0
  , ukInWeight = 0.333333
  , ukOutWeight = 0.333333
  , ukWeightedMode = 0
  }

-- | Convert bone to Unity path
boneToPath :: UnityAnimOptions -> HumanoidBone -> String
boneToPath opts bone =
  let prefix = T.unpack (uaoRootBonePath opts)
      bonePath = boneHierarchyPath bone
  in if null prefix
     then bonePath
     else prefix ++ "/" ++ bonePath

-- | Get bone hierarchy path for Unity
boneHierarchyPath :: HumanoidBone -> String
boneHierarchyPath bone = case bone of
  Hips -> "Hips"
  Spine -> "Hips/Spine"
  Chest -> "Hips/Spine/Chest"
  UpperChest -> "Hips/Spine/Chest/UpperChest"
  Neck -> "Hips/Spine/Chest/UpperChest/Neck"
  Head -> "Hips/Spine/Chest/UpperChest/Neck/Head"
  Jaw -> "Hips/Spine/Chest/UpperChest/Neck/Head/Jaw"
  LeftShoulder -> "Hips/Spine/Chest/UpperChest/LeftShoulder"
  LeftUpperArm -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm"
  LeftLowerArm -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm"
  LeftHand -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand"
  RightShoulder -> "Hips/Spine/Chest/UpperChest/RightShoulder"
  RightUpperArm -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm"
  RightLowerArm -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm"
  RightHand -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand"
  LeftUpperLeg -> "Hips/LeftUpperLeg"
  LeftLowerLeg -> "Hips/LeftUpperLeg/LeftLowerLeg"
  LeftFoot -> "Hips/LeftUpperLeg/LeftLowerLeg/LeftFoot"
  LeftToes -> "Hips/LeftUpperLeg/LeftLowerLeg/LeftFoot/LeftToes"
  RightUpperLeg -> "Hips/RightUpperLeg"
  RightLowerLeg -> "Hips/RightUpperLeg/RightLowerLeg"
  RightFoot -> "Hips/RightUpperLeg/RightLowerLeg/RightFoot"
  RightToes -> "Hips/RightUpperLeg/RightLowerLeg/RightFoot/RightToes"
  LeftEye -> "Hips/Spine/Chest/UpperChest/Neck/Head/LeftEye"
  RightEye -> "Hips/Spine/Chest/UpperChest/Neck/Head/RightEye"
  SpineTwist -> "Hips/Spine/SpineTwist"
  UpperChestTwist -> "Hips/Spine/Chest/UpperChest/UpperChestTwist"
  LeftThumbProximal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftThumbProximal"
  LeftThumbIntermediate -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftThumbProximal/LeftThumbIntermediate"
  LeftThumbDistal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftThumbProximal/LeftThumbIntermediate/LeftThumbDistal"
  LeftIndexProximal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftIndexProximal"
  LeftIndexIntermediate -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftIndexProximal/LeftIndexIntermediate"
  LeftIndexDistal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftIndexProximal/LeftIndexIntermediate/LeftIndexDistal"
  LeftMiddleProximal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftMiddleProximal"
  LeftMiddleIntermediate -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftMiddleProximal/LeftMiddleIntermediate"
  LeftMiddleDistal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftMiddleProximal/LeftMiddleIntermediate/LeftMiddleDistal"
  LeftRingProximal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftRingProximal"
  LeftRingIntermediate -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftRingProximal/LeftRingIntermediate"
  LeftRingDistal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftRingProximal/LeftRingIntermediate/LeftRingDistal"
  LeftLittleProximal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftLittleProximal"
  LeftLittleIntermediate -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftLittleProximal/LeftLittleIntermediate"
  LeftLittleDistal -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand/LeftLittleProximal/LeftLittleIntermediate/LeftLittleDistal"
  RightThumbProximal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightThumbProximal"
  RightThumbIntermediate -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightThumbProximal/RightThumbIntermediate"
  RightThumbDistal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightThumbProximal/RightThumbIntermediate/RightThumbDistal"
  RightIndexProximal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightIndexProximal"
  RightIndexIntermediate -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightIndexProximal/RightIndexIntermediate"
  RightIndexDistal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightIndexProximal/RightIndexIntermediate/RightIndexDistal"
  RightMiddleProximal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightMiddleProximal"
  RightMiddleIntermediate -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightMiddleProximal/RightMiddleIntermediate"
  RightMiddleDistal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightMiddleProximal/RightMiddleIntermediate/RightMiddleDistal"
  RightRingProximal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightRingProximal"
  RightRingIntermediate -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightRingProximal/RightRingIntermediate"
  RightRingDistal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightRingProximal/RightRingIntermediate/RightRingDistal"
  RightLittleProximal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightLittleProximal"
  RightLittleIntermediate -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightLittleProximal/RightLittleIntermediate"
  RightLittleDistal -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand/RightLittleProximal/RightLittleIntermediate/RightLittleDistal"

-- | Serialize Unity animation clip to YAML format (Generic)
serializeUnityClip :: UnityAnimOptions -> UnityAnimationClip -> ByteString
serializeUnityClip opts clip =
  let prec = uaoPrecision opts
      header = unlines
        [ "%YAML 1.1"
        , "%TAG !u! tag:unity3d.com,2011:"
        , "--- !u!74 &7400000"
        , "AnimationClip:"
        , "  m_ObjectHideFlags: 0"
        , "  m_CorrespondingSourceObject: {fileID: 0}"
        , "  m_PrefabInstance: {fileID: 0}"
        , "  m_PrefabAsset: {fileID: 0}"
        , "  m_Name: " ++ uacName clip
        , "  serializedVersion: 6"
        , "  m_Legacy: 0"
        , "  m_Compressed: 0"
        , "  m_UseHighQualityCurve: 1"
        , "  m_RotationCurves: []"
        , "  m_CompressedRotationCurves: []"
        , "  m_EulerCurves: []"
        ]

      posCurvesSection = formatPositionCurves prec (uacPositionCurves clip)
      rotCurvesSection = formatQuaternionCurves prec (uacRotationCurves clip)
      scaleCurvesSection = "  m_ScaleCurves: []"
      floatCurvesSection = "  m_FloatCurves: []"
      ptrCurvesSection = "  m_PPtrCurves: []"

      footer = formatFooter prec clip

  in BS8.pack $ header
              ++ posCurvesSection
              ++ rotCurvesSection
              ++ scaleCurvesSection ++ "\n"
              ++ floatCurvesSection ++ "\n"
              ++ ptrCurvesSection ++ "\n"
              ++ footer

-- | Serialize Unity animation clip to YAML format (Humanoid/Muscle)
serializeUnityClipHumanoid :: UnityAnimOptions -> UnityAnimationClip -> ByteString
serializeUnityClipHumanoid opts clip =
  let prec = uaoPrecision opts
      header = unlines
        [ "%YAML 1.1"
        , "%TAG !u! tag:unity3d.com,2011:"
        , "--- !u!74 &7400000"
        , "AnimationClip:"
        , "  m_ObjectHideFlags: 0"
        , "  m_CorrespondingSourceObject: {fileID: 0}"
        , "  m_PrefabInstance: {fileID: 0}"
        , "  m_PrefabAsset: {fileID: 0}"
        , "  m_Name: " ++ uacName clip
        , "  serializedVersion: 6"
        , "  m_Legacy: 0"
        , "  m_Compressed: 0"
        , "  m_UseHighQualityCurve: 1"
        , "  m_RotationCurves: []"
        , "  m_CompressedRotationCurves: []"
        , "  m_EulerCurves: []"
        , "  m_PositionCurves: []"
        , "  m_ScaleCurves: []"
        ]

      -- Float curves for muscles and root motion (RootT/RootQ)
      allFloatCurves = uacMuscleCurves clip ++ uacRootMotionCurves clip
      floatCurvesSection = formatMuscleCurves prec allFloatCurves

      -- Root motion metadata
      rootMotionSection = formatRootMotion prec (uacRootMotionCurves clip)

      ptrCurvesSection = "  m_PPtrCurves: []"

      footer = formatFooterHumanoid prec clip

  in BS8.pack $ header
              ++ floatCurvesSection
              ++ ptrCurvesSection ++ "\n"
              ++ rootMotionSection
              ++ footer

-- | Format muscle curves section
formatMuscleCurves :: Int -> [UnityCurve] -> String
formatMuscleCurves prec curves =
  if null curves
  then "  m_FloatCurves: []\n"
  else "  m_FloatCurves:\n" ++ concatMap (formatMuscleCurve prec) curves

-- | Format a single muscle curve
formatMuscleCurve :: Int -> UnityCurve -> String
formatMuscleCurve prec curve = unlines
  [ "  - curve:"
  , "      serializedVersion: 2"
  , "      m_Curve:"
  ] ++ formatKeyframes prec (ucKeyframes curve) ++ unlines
  [ "      m_PreInfinity: 2"
  , "      m_PostInfinity: 2"
  , "      m_RotationOrder: 4"
  , "    attribute: " ++ ucPropertyName curve
  , "    path: "
  , "    classID: 95"  -- Animator class ID
  , "    script: {fileID: 0}"
  ]

-- | Format root motion section
formatRootMotion :: Int -> [UnityCurve] -> String
formatRootMotion prec curves =
  if null curves
  then ""
  else let findCurve name = case filter (\c -> ucPropertyName c == name) curves of
             (c:_) -> ucKeyframes c
             [] -> []
       in unlines
         [ "  m_MuscleClip:"
         , "    m_AdditionalCurveIndexArray: []"
         , "    m_IndexArray: []"
         , "    m_AclTransformArray: []"
         , "    m_StartX:"
         , "      m_T: {x: 0, y: 0, z: 0}"
         , "      m_Q: {x: 0, y: 0, z: 0, w: 1}"
         , "    m_StopX:"
         , "      m_T: {x: 0, y: 0, z: 0}"
         , "      m_Q: {x: 0, y: 0, z: 0, w: 1}"
         , "    m_AverageSpeed: {x: 0, y: 0, z: 0}"
         , "    m_Clip:"
         , "      m_StreamedClip:"
         , "        data: []"
         , "        curveCount: 0"
         , "      m_DenseClip:"
         , "        m_FrameCount: " ++ show (length (findCurve "RootT.x"))
         , "        m_CurveCount: " ++ show (length curves)
         , "        m_SampleRate: " ++ showFloat prec 30
         , "        m_BeginTime: 0"
         , "        m_SampleArray: []"
         , "      m_ConstantClip:"
         , "        data: []"
         ]

-- | Format footer (common settings)
formatFooter :: Int -> UnityAnimationClip -> String
formatFooter prec clip = unlines
  [ "  m_SampleRate: " ++ showFloat prec (uacSampleRate clip)
  , "  m_WrapMode: " ++ show (wrapModeToInt (uacWrapMode clip))
  , "  m_Bounds:"
  , "    m_Center: {x: 0, y: 0, z: 0}"
  , "    m_Extent: {x: 0, y: 0, z: 0}"
  , "  m_ClipBindingConstant:"
  , "    genericBindings: []"
  , "    pptrCurveMapping: []"
  , "  m_AnimationClipSettings:"
  , "    serializedVersion: 2"
  , "    m_AdditiveReferencePoseClip: {fileID: 0}"
  , "    m_AdditiveReferencePoseTime: 0"
  , "    m_StartTime: 0"
  , "    m_StopTime: " ++ showFloat prec (getClipDuration clip)
  , "    m_OrientationOffsetY: 0"
  , "    m_Level: 0"
  , "    m_CycleOffset: 0"
  , "    m_HasAdditiveReferencePose: 0"
  , "    m_LoopTime: " ++ (if uacWrapMode clip == WrapLoop then "1" else "0")
  , "    m_LoopBlend: 0"
  , "    m_LoopBlendOrientation: 0"
  , "    m_LoopBlendPositionY: 0"
  , "    m_LoopBlendPositionXZ: 0"
  , "    m_KeepOriginalOrientation: 0"
  , "    m_KeepOriginalPositionY: 1"
  , "    m_KeepOriginalPositionXZ: 0"
  , "    m_HeightFromFeet: 0"
  , "    m_Mirror: 0"
  , "  m_EditorCurves: []"
  , "  m_EulerEditorCurves: []"
  , "  m_HasGenericRootTransform: 0"
  , "  m_HasMotionFloatCurves: 0"
  , "  m_Events: []"
  ]

-- | Format footer for Humanoid clips
formatFooterHumanoid :: Int -> UnityAnimationClip -> String
formatFooterHumanoid prec clip = unlines
  [ "  m_SampleRate: " ++ showFloat prec (uacSampleRate clip)
  , "  m_WrapMode: " ++ show (wrapModeToInt (uacWrapMode clip))
  , "  m_Bounds:"
  , "    m_Center: {x: 0, y: 0, z: 0}"
  , "    m_Extent: {x: 0, y: 0, z: 0}"
  , "  m_ClipBindingConstant:"
  , "    genericBindings: []"
  , "    pptrCurveMapping: []"
  , "  m_AnimationClipSettings:"
  , "    serializedVersion: 2"
  , "    m_AdditiveReferencePoseClip: {fileID: 0}"
  , "    m_AdditiveReferencePoseTime: 0"
  , "    m_StartTime: 0"
  , "    m_StopTime: " ++ showFloat prec (getClipDurationHumanoid clip)
  , "    m_OrientationOffsetY: 0"
  , "    m_Level: 0"
  , "    m_CycleOffset: 0"
  , "    m_HasAdditiveReferencePose: 0"
  , "    m_LoopTime: " ++ (if uacWrapMode clip == WrapLoop then "1" else "0")
  , "    m_LoopBlend: 0"
  , "    m_LoopBlendOrientation: 0"
  , "    m_LoopBlendPositionY: 0"
  , "    m_LoopBlendPositionXZ: 0"
  , "    m_KeepOriginalOrientation: 0"
  , "    m_KeepOriginalPositionY: 1"
  , "    m_KeepOriginalPositionXZ: 0"
  , "    m_HeightFromFeet: 0"
  , "    m_Mirror: 0"
  , "  m_EditorCurves: []"
  , "  m_EulerEditorCurves: []"
  , "  m_HasGenericRootTransform: 0"
  , "  m_HasMotionFloatCurves: 1"
  , "  m_Events: []"
  ]

-- | Get clip duration from curves
getClipDuration :: UnityAnimationClip -> Float
getClipDuration clip =
  let allKeyframes = concatMap ucKeyframes (uacPositionCurves clip ++ uacRotationCurves clip)
  in if null allKeyframes
     then 0
     else maximum (map ukTime allKeyframes)

-- | Get clip duration from humanoid curves
getClipDurationHumanoid :: UnityAnimationClip -> Float
getClipDurationHumanoid clip =
  let allKeyframes = concatMap ucKeyframes (uacMuscleCurves clip ++ uacRootMotionCurves clip)
  in if null allKeyframes
     then 0
     else maximum (map ukTime allKeyframes)

-- | Format position curves section
formatPositionCurves :: Int -> [UnityCurve] -> String
formatPositionCurves prec curves =
  if null curves
  then "  m_PositionCurves: []\n"
  else "  m_PositionCurves:\n" ++ concatMap (formatCurve prec) curves

-- | Format quaternion rotation curves section
formatQuaternionCurves :: Int -> [UnityCurve] -> String
formatQuaternionCurves prec curves =
  if null curves
  then "  m_QuaternionCurves: []\n"
  else "  m_QuaternionCurves:\n" ++ concatMap (formatCurve prec) curves

-- | Format a single curve
formatCurve :: Int -> UnityCurve -> String
formatCurve prec curve = unlines
  [ "  - curve:"
  , "      serializedVersion: 2"
  , "      m_Curve:"
  ] ++ formatKeyframes prec (ucKeyframes curve) ++ unlines
  [ "      m_PreInfinity: 2"
  , "      m_PostInfinity: 2"
  , "      m_RotationOrder: 4"
  , "    path: " ++ ucPath curve
  ]

-- | Format keyframes
formatKeyframes :: Int -> [UnityKeyframe] -> String
formatKeyframes prec kfs = concatMap (formatKeyframe prec) kfs

-- | Format a single keyframe
formatKeyframe :: Int -> UnityKeyframe -> String
formatKeyframe prec kf = unlines
  [ "      - serializedVersion: 3"
  , "        time: " ++ showFloat prec (ukTime kf)
  , "        value: " ++ showFloat prec (ukValue kf)
  , "        inSlope: " ++ showFloat prec (ukInTangent kf)
  , "        outSlope: " ++ showFloat prec (ukOutTangent kf)
  , "        tangentMode: 0"
  , "        weightedMode: " ++ show (ukWeightedMode kf)
  , "        inWeight: " ++ showFloat prec (ukInWeight kf)
  , "        outWeight: " ++ showFloat prec (ukOutWeight kf)
  ]

-- | Convert wrap mode to Unity integer
wrapModeToInt :: WrapMode -> Int
wrapModeToInt WrapOnce = 1
wrapModeToInt WrapLoop = 2
wrapModeToInt WrapPingPong = 4
wrapModeToInt WrapClampForever = 8

-- | Show float with precision
showFloat :: Int -> Float -> String
showFloat prec f = printf ("%." ++ show prec ++ "f") f
