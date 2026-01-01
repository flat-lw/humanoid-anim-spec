-- |
-- Module      : HumanoidAnim.Output.UnityAnim
-- Description : Unity .anim file format export
--
-- This module exports animation clips to Unity's native .anim format,
-- which is a YAML-based serialization format used by Unity's animation system.
module HumanoidAnim.Output.UnityAnim
  ( -- * Export Functions
    exportUnityAnim
  , writeUnityAnim

    -- * Options
  , UnityAnimOptions(..)
  , WrapMode(..)
  , defaultUnityAnimOptions

    -- * Unity Types
  , UnityAnimationClip(..)
  , UnityCurve(..)
  , UnityKeyframe(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Linear (V3(..), Quaternion(..))
import Text.Printf (printf)

import HumanoidAnim.Animation
import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config (Transform(..))

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
  }

-- | Unity animation clip structure
data UnityAnimationClip = UnityAnimationClip
  { uacName :: String
  , uacSampleRate :: Float
  , uacWrapMode :: WrapMode
  , uacPositionCurves :: [UnityCurve]
  , uacRotationCurves :: [UnityCurve]
  , uacScaleCurves :: [UnityCurve]
  } deriving stock (Show, Eq)

-- | Unity animation curve
data UnityCurve = UnityCurve
  { ucPath :: String
    -- ^ Bone path in hierarchy
  , ucPropertyName :: String
    -- ^ Property name (e.g., "m_LocalPosition.x")
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

-- | Export animation clip to Unity .anim format
exportUnityAnim :: UnityAnimOptions -> AnimationClip -> ByteString
exportUnityAnim opts clip =
  let unityClip = animToUnity opts clip
  in serializeUnityClip opts unityClip

-- | Write Unity .anim file
writeUnityAnim :: FilePath -> UnityAnimOptions -> AnimationClip -> IO ()
writeUnityAnim path opts clip = BS.writeFile path (exportUnityAnim opts clip)

-- | Convert AnimationClip to Unity format
animToUnity :: UnityAnimOptions -> AnimationClip -> UnityAnimationClip
animToUnity opts clip =
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
       }

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
-- Matches the actual HumanoidBone constructors defined in Bones.hs
boneHierarchyPath :: HumanoidBone -> String
boneHierarchyPath bone = case bone of
  -- Core body
  Hips -> "Hips"
  Spine -> "Hips/Spine"
  Chest -> "Hips/Spine/Chest"
  UpperChest -> "Hips/Spine/Chest/UpperChest"
  Neck -> "Hips/Spine/Chest/UpperChest/Neck"
  Head -> "Hips/Spine/Chest/UpperChest/Neck/Head"
  Jaw -> "Hips/Spine/Chest/UpperChest/Neck/Head/Jaw"

  -- Left arm
  LeftShoulder -> "Hips/Spine/Chest/UpperChest/LeftShoulder"
  LeftUpperArm -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm"
  LeftLowerArm -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm"
  LeftHand -> "Hips/Spine/Chest/UpperChest/LeftShoulder/LeftUpperArm/LeftLowerArm/LeftHand"

  -- Right arm
  RightShoulder -> "Hips/Spine/Chest/UpperChest/RightShoulder"
  RightUpperArm -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm"
  RightLowerArm -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm"
  RightHand -> "Hips/Spine/Chest/UpperChest/RightShoulder/RightUpperArm/RightLowerArm/RightHand"

  -- Left leg
  LeftUpperLeg -> "Hips/LeftUpperLeg"
  LeftLowerLeg -> "Hips/LeftUpperLeg/LeftLowerLeg"
  LeftFoot -> "Hips/LeftUpperLeg/LeftLowerLeg/LeftFoot"
  LeftToes -> "Hips/LeftUpperLeg/LeftLowerLeg/LeftFoot/LeftToes"

  -- Right leg
  RightUpperLeg -> "Hips/RightUpperLeg"
  RightLowerLeg -> "Hips/RightUpperLeg/RightLowerLeg"
  RightFoot -> "Hips/RightUpperLeg/RightLowerLeg/RightFoot"
  RightToes -> "Hips/RightUpperLeg/RightLowerLeg/RightFoot/RightToes"

  -- Eyes
  LeftEye -> "Hips/Spine/Chest/UpperChest/Neck/Head/LeftEye"
  RightEye -> "Hips/Spine/Chest/UpperChest/Neck/Head/RightEye"

  -- Twist bones
  SpineTwist -> "Hips/Spine/SpineTwist"
  UpperChestTwist -> "Hips/Spine/Chest/UpperChest/UpperChestTwist"

  -- Left hand fingers
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

  -- Right hand fingers
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

-- | Serialize Unity animation clip to YAML format
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

      footer = unlines
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

  in BS8.pack $ header
              ++ posCurvesSection
              ++ rotCurvesSection
              ++ scaleCurvesSection ++ "\n"
              ++ floatCurvesSection ++ "\n"
              ++ ptrCurvesSection ++ "\n"
              ++ footer

-- | Get clip duration from curves
getClipDuration :: UnityAnimationClip -> Float
getClipDuration clip =
  let allKeyframes = concatMap ucKeyframes (uacPositionCurves clip ++ uacRotationCurves clip)
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
