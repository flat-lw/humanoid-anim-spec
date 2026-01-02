# 背骨IK（比例分配方式）統合ガイド

## 概要

背骨IKは、Hipsを固定点として、Spine → Chest → UpperChest → Neck の各関節に回転を分配し、自然な体の傾きを実現します。

既存の2ボーンIK（腕・脚）と組み合わせることで、「手を遠くに伸ばすと体が傾く」といった自然な動きを生成できます。

---

## 背骨の構造

```
Humanoid 背骨チェーン:

         Head        ← 末端（回転の影響を受ける）
          ↑
         Neck        ← 重み 0.15
          ↑
      UpperChest     ← 重み 0.20
          ↑
        Chest        ← 重み 0.30
          ↑
        Spine        ← 重み 0.35
          ↑
        Hips         ← 固定点（回転の中心）

※UpperChestがない場合（MinimalSkeleton）は3ボーン構成
```

---

## 比例分配の原理

### 基本概念

```
全体で必要な回転を、各関節の「重み」に応じて分配する

例: 前方に30度傾ける場合

  全体回転: 30°
  
  分配:
    Spine:      30° × 0.35 = 10.5°
    Chest:      30° × 0.30 =  9.0°
    UpperChest: 30° × 0.20 =  6.0°
    Neck:       30° × 0.15 =  4.5°
    ─────────────────────────────
    合計:                   30.0°
```

### 重みの意味

```
重みが大きい = その関節が大きく動く

人体の特性:
  - Spine, Chest: 可動域が大きい → 重みを大きく
  - UpperChest, Neck: 可動域が小さい → 重みを小さく

重みの合計は1.0になるように正規化する
```

### 累積回転

```
各関節は「自分より下の関節の回転の累積」の影響を受ける

Hips基準で考えると:

  Spine位置      = Hips位置 + (SpineへのOffset)
  Chest位置      = Spine位置を、Spineの回転で変換
  UpperChest位置 = Chest位置を、Spine+Chestの回転で変換
  Neck位置       = UpperChest位置を、Spine+Chest+UpperChestの回転で変換
  Head位置       = Neck位置を、全回転で変換

これにより、下の関節を回すと上の関節も一緒に動く（親子関係）
```

---

## 型定義

```haskell
module HumanoidAnim.IK.Spine where

import Linear.V3
import Linear.Quaternion
import Linear.Vector
import Linear.Metric
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- 既存の型を使用
-- type Pose = Map.Map HumanoidBone (V3 Float)
-- type FullPose = Map.Map HumanoidBone Transform

-- | 背骨ボーンの設定
data SpineBoneConfig = SpineBoneConfig
  { sbcBone   :: !HumanoidBone   -- ボーン
  , sbcWeight :: !Float          -- 回転の重み（0.0-1.0）
  , sbcLimit  :: !Float          -- 最大回転角度（ラジアン）
  } deriving (Show, Eq)

-- | 背骨チェーン設定
data SpineConfig = SpineConfig
  { scBones      :: ![SpineBoneConfig]  -- 下から上の順
  , scStiffness  :: !Float              -- 硬さ（0.0-1.0、1.0で完全に追従）
  } deriving (Show, Eq)

-- | 背骨IKの結果
data SpineIKResult = SpineIKResult
  { sirPose           :: !Pose              -- 更新されたポーズ
  , sirRotations      :: ![Quaternion Float] -- 各関節の回転
  , sirAppliedAngle   :: !Float             -- 実際に適用された角度
  , sirRequestedAngle :: !Float             -- 要求された角度
  } deriving (Show)
```

---

## デフォルト設定

```haskell
-- | 標準的な背骨設定（StandardSkeleton用）
defaultSpineConfig :: SpineConfig
defaultSpineConfig = SpineConfig
  { scBones =
      [ SpineBoneConfig Spine      0.35 (pi/6)   -- 30度制限
      , SpineBoneConfig Chest      0.30 (pi/6)
      , SpineBoneConfig UpperChest 0.20 (pi/8)   -- 22.5度制限
      , SpineBoneConfig Neck       0.15 (pi/6)
      ]
  , scStiffness = 0.8
  }

-- | 最小限の背骨設定（MinimalSkeleton用、UpperChestなし）
minimalSpineConfig :: SpineConfig
minimalSpineConfig = SpineConfig
  { scBones =
      [ SpineBoneConfig Spine 0.40 (pi/5)
      , SpineBoneConfig Chest 0.35 (pi/5)
      , SpineBoneConfig Neck  0.25 (pi/6)
      ]
  , scStiffness = 0.8
  }

-- | スケルトンに応じた設定を取得
getSpineConfig :: Skeleton -> SpineConfig
getSpineConfig skel =
  let activeBones = skeletonActiveBones skel
  in if UpperChest `Set.member` activeBones
     then defaultSpineConfig
     else minimalSpineConfig
```

---

## 回転計算

### 目標方向への回転を計算

```haskell
-- | 2つの方向ベクトル間の回転を計算
--
-- from: 現在の方向（正規化済み）
-- to:   目標の方向（正規化済み）
--
rotationBetweenVectors :: V3 Float -> V3 Float -> Quaternion Float
rotationBetweenVectors from to
  | norm from < 0.0001 || norm to < 0.0001 = identityQuat
  | otherwise =
      let fromN = normalize from
          toN = normalize to
          d = fromN `dot` toN
      in if d > 0.9999
         then identityQuat  -- ほぼ同じ方向
         else if d < -0.9999
         then -- 反対方向: 任意の垂直軸で180度回転
              let axis = findPerpendicular fromN
              in axisAngle axis pi
         else
           let axis = normalize (fromN `cross` toN)
               angle = acos (clamp (-1) 1 d)
           in axisAngle axis angle

-- | 垂直なベクトルを見つける
findPerpendicular :: V3 Float -> V3 Float
findPerpendicular v =
  let v1 = v `cross` (V3 1 0 0)
      v2 = v `cross` (V3 0 1 0)
  in if quadrance v1 > quadrance v2
     then normalize v1
     else normalize v2

-- | 単位クォータニオン
identityQuat :: Quaternion Float
identityQuat = Quaternion 1 (V3 0 0 0)

-- | クランプ
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)
```

### 回転の分解と制限

```haskell
-- | クォータニオンを軸と角度に分解
quaternionToAxisAngle :: Quaternion Float -> (V3 Float, Float)
quaternionToAxisAngle (Quaternion w (V3 x y z)) =
  let angle = 2 * acos (clamp (-1) 1 w)
      s = sqrt (1 - w * w)
      axis = if s < 0.0001
             then V3 1 0 0  -- 任意の軸
             else V3 (x/s) (y/s) (z/s)
  in (axis, angle)

-- | 回転角度を制限
limitRotation :: Float -> Quaternion Float -> Quaternion Float
limitRotation maxAngle rot =
  let (axis, angle) = quaternionToAxisAngle rot
      limitedAngle = clamp (-maxAngle) maxAngle angle
  in if abs angle < 0.0001
     then identityQuat
     else axisAngle axis limitedAngle

-- | 回転を部分的に適用（球面線形補間）
partialRotation :: Float -> Quaternion Float -> Quaternion Float
partialRotation t rot
  | t <= 0    = identityQuat
  | t >= 1    = rot
  | otherwise = slerp identityQuat rot t

-- | 球面線形補間
slerp :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float
slerp q1 q2 t =
  let d = q1 `quatDot` q2
      -- 最短経路を取るため、必要なら符号反転
      (q2', d') = if d < 0 
                  then (negateQuat q2, -d) 
                  else (q2, d)
      theta = acos (clamp (-1) 1 d')
  in if theta < 0.0001
     then q1  -- ほぼ同じ
     else let sinTheta = sin theta
              a = sin ((1 - t) * theta) / sinTheta
              b = sin (t * theta) / sinTheta
          in (q1 ^* a) + (q2' ^* b)

-- | クォータニオンの内積
quatDot :: Quaternion Float -> Quaternion Float -> Float
quatDot (Quaternion w1 v1) (Quaternion w2 v2) = w1 * w2 + (v1 `dot` v2)

-- | クォータニオンの符号反転
negateQuat :: Quaternion Float -> Quaternion Float
negateQuat (Quaternion w v) = Quaternion (-w) (negate v)
```

---

## 背骨IKソルバー

### メイン関数

```haskell
-- | 背骨IKを解く
--
-- 引数:
--   skel   : スケルトン定義
--   pose   : 現在のポーズ（位置のみ）
--   config : 背骨設定
--   target : 背骨末端（Neckの上端）の目標位置
--
-- 戻り値:
--   更新されたポーズと回転情報
--
solveSpineIK
  :: Skeleton
  -> Pose
  -> SpineConfig
  -> V3 Float        -- 目標位置
  -> SpineIKResult
solveSpineIK skel pose config target =
  let -- Hipsの位置（回転の中心、固定）
      hipsPos = fromMaybe (V3 0 1 0) $ Map.lookup Hips pose
      
      -- 現在の背骨末端位置
      topBone = sbcBone $ last (scBones config)
      currentTop = fromMaybe hipsPos $ Map.lookup topBone pose
      
      -- Hipsから見た現在の方向と目標方向
      currentDir = normalize (currentTop - hipsPos)
      targetDir = normalize (target - hipsPos)
      
      -- 全体の必要回転
      totalRotation = rotationBetweenVectors currentDir targetDir
      
      -- stiffnessを適用
      adjustedRotation = partialRotation (scStiffness config) totalRotation
      
      -- 全体の回転角度
      (_, totalAngle) = quaternionToAxisAngle adjustedRotation
      
      -- 各ボーンに分配
      (newPose, rotations, appliedAngle) = 
        distributeRotation skel pose hipsPos config adjustedRotation
      
  in SpineIKResult
       { sirPose = newPose
       , sirRotations = rotations
       , sirAppliedAngle = appliedAngle
       , sirRequestedAngle = totalAngle
       }
```

### 回転の分配

```haskell
-- | 回転を各ボーンに分配
distributeRotation
  :: Skeleton
  -> Pose
  -> V3 Float           -- Hips位置（固定点）
  -> SpineConfig
  -> Quaternion Float   -- 全体の回転
  -> (Pose, [Quaternion Float], Float)
distributeRotation skel pose hipsPos config totalRotation =
  let bones = scBones config
      
      -- 重みを正規化
      totalWeight = sum (map sbcWeight bones)
      normalizedBones = 
        [ sbc { sbcWeight = sbcWeight sbc / totalWeight }
        | sbc <- bones
        ]
      
      -- 全体の角度
      (rotAxis, totalAngle) = quaternionToAxisAngle totalRotation
      
      -- 各ボーンの回転を計算（累積）
      (finalPose, rotations, _) = 
        foldl' (applyBoneRotation hipsPos rotAxis totalAngle)
               (pose, [], identityQuat)
               normalizedBones
      
      appliedAngle = sum 
        [ let (_, a) = quaternionToAxisAngle r in a 
        | r <- rotations 
        ]
      
  in (finalPose, rotations, appliedAngle)

-- | 1つのボーンに回転を適用
applyBoneRotation
  :: V3 Float           -- Hips位置
  -> V3 Float           -- 回転軸
  -> Float              -- 全体の角度
  -> (Pose, [Quaternion Float], Quaternion Float)  -- (現在のポーズ, 回転リスト, 累積回転)
  -> SpineBoneConfig
  -> (Pose, [Quaternion Float], Quaternion Float)
applyBoneRotation hipsPos rotAxis totalAngle (pose, rotList, accumRot) boneConfig =
  let bone = sbcBone boneConfig
      weight = sbcWeight boneConfig
      limit = sbcLimit boneConfig
      
      -- このボーンの回転角度
      boneAngle = totalAngle * weight
      limitedAngle = clamp (-limit) limit boneAngle
      
      -- このボーンの回転
      boneRotation = if abs limitedAngle < 0.0001
                     then identityQuat
                     else axisAngle rotAxis limitedAngle
      
      -- 累積回転を更新
      newAccumRot = boneRotation * accumRot
      
      -- このボーンの新しい位置を計算
      currentPos = fromMaybe hipsPos $ Map.lookup bone pose
      
      -- Hipsからの相対位置を累積回転で変換
      relativePos = currentPos - hipsPos
      newRelativePos = rotate newAccumRot relativePos
      newPos = hipsPos + newRelativePos
      
      -- ポーズを更新
      newPose = Map.insert bone newPos pose
      
  in (newPose, rotList ++ [boneRotation], newAccumRot)
```

---

## 子ボーンの追従

背骨を動かすと、その上にある全てのボーン（腕、頭など）も一緒に動く必要があります。

```haskell
-- | 背骨の回転に合わせて子ボーンを更新
--
-- 背骨IKの後に呼び出す
--
updateChildBones
  :: Skeleton
  -> Pose
  -> V3 Float             -- Hips位置
  -> Quaternion Float     -- 背骨の累積回転
  -> Pose
updateChildBones skel pose hipsPos spineRotation =
  let -- 背骨の影響を受けるボーン
      affectedBones = 
        [ Head
        , LeftShoulder, LeftUpperArm, LeftLowerArm, LeftHand
        , RightShoulder, RightUpperArm, RightLowerArm, RightHand
        ]
      
      -- 各ボーンを回転
      updateBone p bone =
        case Map.lookup bone p of
          Nothing -> p
          Just pos ->
            let relPos = pos - hipsPos
                newRelPos = rotate spineRotation relPos
                newPos = hipsPos + newRelPos
            in Map.insert bone newPos p
      
  in foldl' updateBone pose affectedBones

-- | 背骨IKの完全な処理
solveSpineIKFull
  :: Skeleton
  -> Pose
  -> SpineConfig
  -> V3 Float
  -> Pose
solveSpineIKFull skel pose config target =
  let result = solveSpineIK skel pose config target
      hipsPos = fromMaybe (V3 0 1 0) $ Map.lookup Hips pose
      
      -- 累積回転を計算
      accumRotation = foldl' (*) identityQuat (sirRotations result)
      
      -- 子ボーンを更新
      finalPose = updateChildBones skel (sirPose result) hipsPos accumRotation
      
  in finalPose
```

---

## 全身IKとの統合

### 処理フロー

```
┌─────────────────────────────────────────────────────────┐
│                    全身IK処理フロー                      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. 入力                                                │
│     ├─ 固定制約: Hips, LeftFoot, RightFoot             │
│     └─ エフェクター: RightHand の目標位置              │
│                                                         │
│  2. 固定ボーンを適用                                    │
│     └─ Hips, 両足の位置を設定                          │
│                                                         │
│  3. 到達判定                                            │
│     ├─ 手の目標が腕だけで届く？                        │
│     │   └─ Yes → 手順5へスキップ                       │
│     └─ No → 背骨IKが必要                               │
│                                                         │
│  4. 背骨IKを解く                                        │
│     ├─ 肩を目標に近づける方向に体を傾ける              │
│     └─ 子ボーン（腕など）も一緒に移動                  │
│                                                         │
│  5. 腕IKを解く                                          │
│     └─ 更新された肩位置から手を目標へ                  │
│                                                         │
│  6. 回転を計算                                          │
│     └─ 全ボーンの位置から回転を導出                    │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### 統合ソルバー

```haskell
module HumanoidAnim.IK.FullBody where

import HumanoidAnim.IK.TwoBone
import HumanoidAnim.IK.Spine

-- | 全身IK設定
data FullBodyIKConfig = FullBodyIKConfig
  { fbSpineConfig      :: !SpineConfig
  , fbArmReachFactor   :: !Float   -- 腕だけで届く割合（0.9など）
  , fbSpineContribution :: !Float  -- 背骨が補う割合（0.5など）
  } deriving (Show, Eq)

defaultFullBodyConfig :: Skeleton -> FullBodyIKConfig
defaultFullBodyConfig skel = FullBodyIKConfig
  { fbSpineConfig = getSpineConfig skel
  , fbArmReachFactor = 0.90      -- 腕の90%を超えたら背骨を使う
  , fbSpineContribution = 0.5   -- 不足分の50%を背骨で補う
  }

-- | 全身IKを解く
solveFullBodyIK
  :: Skeleton
  -> FullBodyIKConfig
  -> Pose                    -- 初期ポーズ
  -> [IKConstraint]          -- 制約
  -> Pose
solveFullBodyIK skel config initialPose constraints =
  let -- 1. 固定制約を適用
      pose1 = applyFixedConstraints constraints initialPose
      
      -- 2. エフェクター制約を分類
      armEffectors = [(side, pos) | Effector bone pos <- constraints
                                  , side <- boneToArmSide bone]
      legEffectors = [(side, pos) | Effector bone pos <- constraints
                                  , side <- boneToLegSide bone]
      
      -- 3. 脚IKを先に解く（足は通常固定）
      pose2 = foldl' (solveLegConstraint skel) pose1 legEffectors
      
      -- 4. 腕IKを解く（背骨統合あり）
      pose3 = foldl' (solveArmWithSpine skel config) pose2 armEffectors
      
  in pose3

-- | ボーンから腕のサイドを判定
boneToArmSide :: HumanoidBone -> [ArmSide]
boneToArmSide LeftHand = [LeftArm]
boneToArmSide RightHand = [RightArm]
boneToArmSide _ = []

-- | ボーンから脚のサイドを判定
boneToLegSide :: HumanoidBone -> [LegSide]
boneToLegSide LeftFoot = [LeftLeg]
boneToLegSide RightFoot = [RightLeg]
boneToLegSide _ = []

data ArmSide = LeftArm | RightArm deriving (Show, Eq)
data LegSide = LeftLeg | RightLeg deriving (Show, Eq)
```

### 背骨を使った腕IK

```haskell
-- | 腕IKを解く（必要に応じて背骨も動かす）
solveArmWithSpine
  :: Skeleton
  -> FullBodyIKConfig
  -> Pose
  -> (ArmSide, V3 Float)     -- (腕のサイド, 手の目標位置)
  -> Pose
solveArmWithSpine skel config pose (armSide, target) =
  let -- 肩の位置を取得
      shoulderBone = case armSide of
        LeftArm  -> LeftShoulder
        RightArm -> RightShoulder
      shoulderPos = fromMaybe (V3 0 1.4 0) $ Map.lookup shoulderBone pose
      
      -- 腕の長さ
      (upperBone, lowerBone) = case armSide of
        LeftArm  -> (LeftUpperArm, LeftLowerArm)
        RightArm -> (RightUpperArm, RightLowerArm)
      armLength = getBoneLength skel upperBone + getBoneLength skel lowerBone
      
      -- 目標までの距離
      distToTarget = distance shoulderPos target
      
      -- 腕だけで届くか判定
      reachThreshold = armLength * fbArmReachFactor config
      
  in if distToTarget <= reachThreshold
     -- 腕だけで届く
     then solveArmOnly skel pose armSide target
     -- 背骨の補助が必要
     else solveArmWithSpineAssist skel config pose armSide target distToTarget armLength

-- | 腕のみでIKを解く
solveArmOnly :: Skeleton -> Pose -> ArmSide -> V3 Float -> Pose
solveArmOnly skel pose armSide target =
  let (shoulderBone, upperBone, lowerBone, handBone) = armBones armSide
      
      shoulderPos = fromMaybe (V3 0 1.4 0) $ Map.lookup shoulderBone pose
      upperLen = getBoneLength skel upperBone
      lowerLen = getBoneLength skel lowerBone
      
      -- 肘のヒント（後方）
      hint = V3 0 (-0.3) (-1)
      
      result = solveTwoBone shoulderPos upperLen lowerLen target hint
      
  in case result of
       Solved elbowPos handPos ->
         pose
           & Map.insert upperBone elbowPos
           & Map.insert lowerBone handPos
           & Map.insert handBone target
       
       Unreachable elbowPos handPos _ ->
         pose
           & Map.insert upperBone elbowPos
           & Map.insert lowerBone handPos
           & Map.insert handBone handPos

-- | 背骨を使って腕IKを解く
solveArmWithSpineAssist
  :: Skeleton
  -> FullBodyIKConfig
  -> Pose
  -> ArmSide
  -> V3 Float    -- 手の目標
  -> Float       -- 現在の距離
  -> Float       -- 腕の長さ
  -> Pose
solveArmWithSpineAssist skel config pose armSide target dist armLength =
  let -- 不足距離
      shortfall = dist - armLength * fbArmReachFactor config
      
      -- 背骨で補う量
      spineContrib = shortfall * fbSpineContribution config
      
      -- 肩の現在位置
      shoulderBone = case armSide of
        LeftArm  -> LeftShoulder
        RightArm -> RightShoulder
      shoulderPos = fromMaybe (V3 0 1.4 0) $ Map.lookup shoulderBone pose
      
      -- 肩を目標方向に移動させる目標位置
      dirToTarget = normalize (target - shoulderPos)
      newShoulderTarget = shoulderPos + dirToTarget ^* spineContrib
      
      -- 背骨IKで体を傾ける
      hipsPos = fromMaybe (V3 0 1 0) $ Map.lookup Hips pose
      
      -- 背骨の目標は、現在のNeck位置 + オフセット
      neckPos = fromMaybe (V3 0 1.5 0) $ Map.lookup Neck pose
      spineTarget = neckPos + dirToTarget ^* spineContrib
      
      -- 背骨IKを解く
      pose' = solveSpineIKFull skel pose (fbSpineConfig config) spineTarget
      
      -- 更新された肩位置で腕IKを解く
      pose'' = solveArmOnly skel pose' armSide target
      
  in pose''

-- | 腕のボーン一覧を取得
armBones :: ArmSide -> (HumanoidBone, HumanoidBone, HumanoidBone, HumanoidBone)
armBones LeftArm  = (LeftShoulder, LeftUpperArm, LeftLowerArm, LeftHand)
armBones RightArm = (RightShoulder, RightUpperArm, RightLowerArm, RightHand)
```

### 脚IK

```haskell
-- | 脚IKを解く
solveLegConstraint :: Skeleton -> Pose -> (LegSide, V3 Float) -> Pose
solveLegConstraint skel pose (legSide, target) =
  let (upperBone, lowerBone, footBone) = case legSide of
        LeftLeg  -> (LeftUpperLeg, LeftLowerLeg, LeftFoot)
        RightLeg -> (RightUpperLeg, RightLowerLeg, RightFoot)
      
      -- 股関節の位置（Hipsからのオフセット）
      hipsPos = fromMaybe (V3 0 1 0) $ Map.lookup Hips pose
      hipOffset = case legSide of
        LeftLeg  -> V3 0.1 (-0.05) 0
        RightLeg -> V3 (-0.1) (-0.05) 0
      hipJointPos = hipsPos + hipOffset
      
      upperLen = getBoneLength skel upperBone
      lowerLen = getBoneLength skel lowerBone
      
      -- 膝のヒント（前方）
      hint = V3 0 0 1
      
      result = solveTwoBone hipJointPos upperLen lowerLen target hint
      
  in case result of
       Solved kneePos footPos ->
         pose
           & Map.insert upperBone kneePos
           & Map.insert lowerBone footPos
           & Map.insert footBone target
       
       Unreachable kneePos footPos _ ->
         pose
           & Map.insert upperBone kneePos
           & Map.insert lowerBone footPos
           & Map.insert footBone footPos
```

---

## アニメーション生成との統合

```haskell
module HumanoidAnim.Animation where

-- | 1フレームを生成
generateFrame
  :: Skeleton
  -> FullBodyIKConfig
  -> [IKConstraint]          -- 固定制約（毎フレーム同じ）
  -> HumanoidBone            -- エフェクターボーン
  -> V3 Float                -- エフェクター目標位置（このフレームの）
  -> FullPose
generateFrame skel config fixedConstraints effectorBone targetPos =
  let -- 初期ポーズ
      initialPose = posePositions $ skeletonRestPose skel
      
      -- 全制約
      allConstraints = fixedConstraints ++ [Effector effectorBone targetPos]
      
      -- 全身IKを解く
      solvedPositions = solveFullBodyIK skel config initialPose allConstraints
      
      -- 位置から回転を計算
      solvedRotations = computeAllRotations skel solvedPositions
      
      -- フルポーズを構築
      fullPose = combinePose solvedPositions solvedRotations
      
  in fullPose

-- | アニメーション全体を生成
generateAnimation
  :: Skeleton
  -> FullBodyIKConfig
  -> [IKConstraint]          -- 固定制約
  -> HumanoidBone            -- エフェクターボーン
  -> [Keyframe]              -- エフェクターのキーフレーム
  -> AnimationConfig
  -> AnimationClip
generateAnimation skel ikConfig fixedConstraints effector keyframes animConfig =
  let duration = maximum (map keyTime keyframes)
      
      -- フレーム数を計算
      frameCount = case acFrameCount animConfig of
        Just n  -> n
        Nothing -> ceiling (duration * acFrameRate animConfig)
      
      -- 各フレームの時間
      times = [fromIntegral i * duration / fromIntegral (frameCount - 1) 
              | i <- [0 .. frameCount - 1]]
      
      -- 軌道関数を作成
      trajectory = keyframesToTrajectory keyframes duration
      
      -- 各フレームを生成
      frames = [ AnimationFrame t (generateFrame skel ikConfig fixedConstraints effector (trajectory t))
               | t <- times
               ]
      
  in AnimationClip
       { clipName = acName animConfig
       , clipDuration = duration
       , clipFrameRate = acFrameRate animConfig
       , clipFrames = frames
       , clipLoopMode = acLoopMode animConfig
       }
```

---

## 使用例

### YAML設定

```yaml
name: "ReachForward"
duration: 2.0

fixed:
  - bone: "Hips"
    position: [0, 1, 0]
  - bone: "LeftFoot"
    position: [-0.1, 0, 0]
  - bone: "RightFoot"
    position: [0.1, 0, 0]

effector:
  bone: "RightHand"
  keyframes:
    - time: 0.0
      position: [0.3, 1.2, 0.2]     # 自然な位置
    - time: 1.0
      position: [0.6, 1.3, 0.8]     # 前方に大きく伸ばす → 体が傾く
    - time: 2.0
      position: [0.3, 1.2, 0.2]     # 戻る

config:
  # 背骨IK設定
  spine:
    stiffness: 0.8
  
  # 統合IK設定
  armReachFactor: 0.9
  spineContribution: 0.5
```

### 生成結果イメージ

```
時間 0.0:                時間 1.0:                時間 2.0:
(自然な姿勢)             (前に手を伸ばす)          (戻る)

     O                        O                       O
     |                       /                        |
    /|\                     /|                       /|\
   / | \                   / |                      / | \
     |                    |  |                        |
    / \                   |  |                       / \
   /   \                  |  |                      /   \
  /     \                 |  ★                     /     \
                         (手が遠いので
                          体が前傾)
```

---

## テスト

```haskell
-- | 背骨IKのテスト
testSpineIK :: IO ()
testSpineIK = do
  let skel = buildDefaultSkeleton
      config = defaultSpineConfig
      initialPose = posePositions $ skeletonRestPose skel
      
      -- Neckを前方に移動させる目標
      neckPos = initialPose Map.! Neck
      target = neckPos + V3 0 0 0.3  -- 30cm前
      
      result = solveSpineIK skel initialPose config target
      
  putStrLn $ "Requested angle: " ++ show (sirRequestedAngle result)
  putStrLn $ "Applied angle: " ++ show (sirAppliedAngle result)
  putStrLn $ "Rotations: " ++ show (length $ sirRotations result)
  
  -- Neckが目標に近づいているか
  let newNeckPos = sirPose result Map.! Neck
      improvement = distance neckPos target - distance newNeckPos target
  
  putStrLn $ "Improvement: " ++ show improvement ++ " meters"
  assert (improvement > 0) "Neck should be closer to target"

-- | 統合IKのテスト
testFullBodyIK :: IO ()
testFullBodyIK = do
  let skel = buildDefaultSkeleton
      config = defaultFullBodyConfig skel
      initialPose = posePositions $ skeletonRestPose skel
      
      -- 遠くの目標
      target = V3 0.8 1.3 0.6  -- 腕の長さを超える位置
      
      constraints = 
        [ Fixed Hips (V3 0 1 0)
        , Fixed LeftFoot (V3 (-0.1) 0 0)
        , Fixed RightFoot (V3 0.1 0 0)
        ]
      
      result = solveFullBodyIK skel config initialPose 
                 (constraints ++ [Effector RightHand target])
      
      handPos = result Map.! RightHand
      chestPos = result Map.! Chest
      initialChestPos = initialPose Map.! Chest
      
  -- 手が目標に近づいているか
  putStrLn $ "Hand position: " ++ show handPos
  putStrLn $ "Distance to target: " ++ show (distance handPos target)
  
  -- 体が傾いているか（Chestが前に出ている）
  let chestForward = (chestPos ^. _z) - (initialChestPos ^. _z)
  putStrLn $ "Chest forward movement: " ++ show chestForward
  assert (chestForward > 0) "Chest should move forward"
```

---

## パラメータ調整ガイド

| パラメータ | 効果 | 推奨値 |
|-----------|------|--------|
| `scStiffness` | 背骨の硬さ。低いと動きが小さい | 0.7 - 0.9 |
| `sbcWeight` | 各関節の動く量の割合 | 合計1.0 |
| `sbcLimit` | 関節の最大角度 | π/6 - π/4 |
| `fbArmReachFactor` | 腕の何%まで背骨を使わないか | 0.85 - 0.95 |
| `fbSpineContribution` | 不足分の何%を背骨で補うか | 0.4 - 0.6 |

### 調整例

```haskell
-- 硬めの動き（リアル寄り）
stiffConfig = defaultFullBodyConfig skel
  { fbSpineConfig = (fbSpineConfig $ defaultFullBodyConfig skel)
      { scStiffness = 0.6 }
  , fbSpineContribution = 0.3
  }

-- 柔らかい動き（アニメ寄り）
flexibleConfig = defaultFullBodyConfig skel
  { fbSpineConfig = (fbSpineConfig $ defaultFullBodyConfig skel)
      { scStiffness = 0.95 }
  , fbSpineContribution = 0.7
  }
```
