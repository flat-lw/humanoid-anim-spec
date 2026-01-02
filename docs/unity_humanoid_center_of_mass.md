# Unity Humanoid 質量中心と正規化メートル

## 概要

UnityのMecanim Humanoidシステムは、異なる身長・プロポーションのキャラクター間でアニメーションをリターゲティングするために、**質量中心（Center of Mass）** と **正規化メートル（Normalized Meters）** という概念を使用しています。

---

## 質量中心（Center of Mass）

### 定義

質量中心は、人体の各部位の質量分布を考慮した重心位置です。Mecanimはこの質量中心をHumanoidのワールド空間位置の基準として使用します。

### 使用理由

従来のスケルトンリグでは、Hips（骨盤）ボーンの位置をルートとして使用していました。しかし、リグによってHipsの相対位置が異なるため、リターゲティング時に問題が発生します。

質量中心を使用する利点：
- 全てのHumanoid Rigで一貫した基準点
- 動的なモーション（バク転など）でも安定
- スケルトン構造に依存しない

### 計算方法

質量中心は各ボーンの位置を質量比率で加重平均して算出します。

```
CoM = Σ(BonePosition × MassRatio) / Σ(MassRatio)
```

各軸について：
```
CoM.x = Σ(Bone.x × MassRatio) / Σ(MassRatio)
CoM.y = Σ(Bone.y × MassRatio) / Σ(MassRatio)
CoM.z = Σ(Bone.z × MassRatio) / Σ(MassRatio)
```

### ボーン質量比率

Unityは「人間の平均的な体部位の質量分布」を使用しています。具体的な値は非公開ですが、バイオメカニクスの標準データ（de Leva 1996, Dempster 1955）に基づくと推定されます。

| 体部位 | 質量比率（%） | 備考 |
|-------|-------------|------|
| 頭部 + 首 | 8.1% | Head, Neck |
| 体幹（上部） | 16.2% | Chest, UpperChest |
| 体幹（中部） | 16.8% | Spine |
| 体幹（下部） | 11.7% | Hips |
| 上腕（片側） | 2.7% | UpperArm |
| 前腕（片側） | 1.6% | LowerArm |
| 手（片側） | 0.6% | Hand |
| 大腿（片側） | 10.0% | UpperLeg |
| 下腿（片側） | 4.3% | LowerLeg |
| 足（片側） | 1.4% | Foot |

**合計**: 100%（左右対称部位は両側合計）

### 質量中心の高さ

T-Pose時の質量中心は概ね：
- 地面から身長の約 **55%** の高さ
- 解剖学的には**へそ付近**（第2仙椎の前方）
- Hips（骨盤）ボーンの近傍

---

## HumanBodyBonesを使った具体的な計算

### 使用するボーンと質量比率

以下のHumanBodyBonesと質量比率を使用して質量中心を計算します。

| HumanBodyBones | 質量比率 | 備考 |
|----------------|---------|------|
| Hips | 0.117 | 骨盤・下部体幹 |
| Spine | 0.084 | 背骨（体幹中部の半分） |
| Chest | 0.084 | 胸（体幹中部の半分） |
| UpperChest | 0.081 | 上胸（オプション、なければChestに統合） |
| Neck | 0.0405 | 首（頭部+首の半分） |
| Head | 0.0405 | 頭（頭部+首の半分） |
| LeftUpperArm | 0.027 | 左上腕 |
| LeftLowerArm | 0.016 | 左前腕 |
| LeftHand | 0.006 | 左手 |
| RightUpperArm | 0.027 | 右上腕 |
| RightLowerArm | 0.016 | 右前腕 |
| RightHand | 0.006 | 右手 |
| LeftUpperLeg | 0.100 | 左大腿 |
| LeftLowerLeg | 0.043 | 左下腿 |
| LeftFoot | 0.014 | 左足 |
| RightUpperLeg | 0.100 | 右大腿 |
| RightLowerLeg | 0.043 | 右下腿 |
| RightFoot | 0.014 | 右足 |

**合計**: 1.0（100%）

### ボーン位置の取得

各ボーンの位置は、そのボーンのTransformのワールド座標を使用します。

```
BonePosition[i] = GetBoneTransform(HumanBodyBones[i]).position
```

### 質量中心の計算手順

#### Step 1: 各ボーンの加重位置を計算

```
WeightedPosition[i] = BonePosition[i] × MassRatio[i]
```

#### Step 2: 加重位置の総和を計算

```
SumWeightedPosition = Σ WeightedPosition[i]
                    = Σ (BonePosition[i] × MassRatio[i])
```

#### Step 3: 質量比率の総和を計算

```
SumMassRatio = Σ MassRatio[i]
             = 1.0（全ボーンが存在する場合）
```

#### Step 4: 質量中心を算出

```
CenterOfMass = SumWeightedPosition / SumMassRatio
```

### 具体的な計算式（展開形）

```
CenterOfMass = (
    Hips.position × 0.117 +
    Spine.position × 0.084 +
    Chest.position × 0.084 +
    UpperChest.position × 0.081 +
    Neck.position × 0.0405 +
    Head.position × 0.0405 +
    LeftUpperArm.position × 0.027 +
    LeftLowerArm.position × 0.016 +
    LeftHand.position × 0.006 +
    RightUpperArm.position × 0.027 +
    RightLowerArm.position × 0.016 +
    RightHand.position × 0.006 +
    LeftUpperLeg.position × 0.100 +
    LeftLowerLeg.position × 0.043 +
    LeftFoot.position × 0.014 +
    RightUpperLeg.position × 0.100 +
    RightLowerLeg.position × 0.043 +
    RightFoot.position × 0.014
) / 1.0
```

### オプションボーンの処理

一部のボーン（UpperChest, Neckなど）はオプションであり、存在しない場合があります。

#### UpperChestがない場合

UpperChestの質量をChestに統合：
```
Chest.massRatio = 0.084 + 0.081 = 0.165
```

#### Neckがない場合

Neckの質量をHeadに統合：
```
Head.massRatio = 0.0405 + 0.0405 = 0.081
```

#### 一般的な処理

```
adjustedMassRatio = originalMassRatio / SumOfExistingBoneMassRatios
```

存在するボーンの質量比率の合計で正規化することで、欠損ボーンの影響を補正します。

### Y座標のみの計算（足接地用）

RootT.yの計算には質量中心のY座標のみが必要です。

```
CenterOfMass.y = (
    Hips.y × 0.117 +
    Spine.y × 0.084 +
    Chest.y × 0.084 +
    UpperChest.y × 0.081 +
    Neck.y × 0.0405 +
    Head.y × 0.0405 +
    LeftUpperArm.y × 0.027 +
    LeftLowerArm.y × 0.016 +
    LeftHand.y × 0.006 +
    RightUpperArm.y × 0.027 +
    RightLowerArm.y × 0.016 +
    RightHand.y × 0.006 +
    LeftUpperLeg.y × 0.100 +
    LeftLowerLeg.y × 0.043 +
    LeftFoot.y × 0.014 +
    RightUpperLeg.y × 0.100 +
    RightLowerLeg.y × 0.043 +
    RightFoot.y × 0.014
) / 1.0
```

### 計算例

T-Poseで直立しているキャラクター（身長1.7m）の場合：

| ボーン | Y座標 (m) | 質量比率 | 加重Y |
|-------|----------|---------|-------|
| Hips | 1.00 | 0.117 | 0.117 |
| Spine | 1.10 | 0.084 | 0.092 |
| Chest | 1.25 | 0.084 | 0.105 |
| UpperChest | 1.35 | 0.081 | 0.109 |
| Neck | 1.50 | 0.0405 | 0.061 |
| Head | 1.60 | 0.0405 | 0.065 |
| LeftUpperArm | 1.40 | 0.027 | 0.038 |
| LeftLowerArm | 1.40 | 0.016 | 0.022 |
| LeftHand | 1.40 | 0.006 | 0.008 |
| RightUpperArm | 1.40 | 0.027 | 0.038 |
| RightLowerArm | 1.40 | 0.016 | 0.022 |
| RightHand | 1.40 | 0.006 | 0.008 |
| LeftUpperLeg | 0.80 | 0.100 | 0.080 |
| LeftLowerLeg | 0.40 | 0.043 | 0.017 |
| LeftFoot | 0.05 | 0.014 | 0.001 |
| RightUpperLeg | 0.80 | 0.100 | 0.080 |
| RightLowerLeg | 0.40 | 0.043 | 0.017 |
| RightFoot | 0.05 | 0.014 | 0.001 |
| **合計** | - | **1.000** | **0.981** |

```
CenterOfMass.y = 0.981 / 1.0 = 0.981m
humanScale = 0.981m
```

### RootT.yの計算例

上記のキャラクターで、しゃがんで質量中心が0.6mになった場合：

```
footY = min(LeftFoot.y, RightFoot.y) = 0.0m（接地）
humanScale = 0.981m

RootT.y = (CenterOfMass.y - footY) / humanScale
        = (0.6 - 0.0) / 0.981
        = 0.612
```

---

## 平均ボディ方向（Average Body Orientation）

### 定義

質量中心が位置を表すのに対し、平均ボディ方向はHumanoidの向きを表します。

### 計算方法

```
上方向ベクトル（Up）:
  hipsMidpoint = (LeftHip + RightHip) / 2
  shoulderMidpoint = (LeftShoulder + RightShoulder) / 2
  up = normalize(shoulderMidpoint - hipsMidpoint)

左右ベクトル（Right）:
  hipsRight = RightHip - LeftHip
  shoulderRight = RightShoulder - LeftShoulder
  right = normalize((hipsRight + shoulderRight) / 2)

前方ベクトル（Forward）:
  forward = cross(up, right)
```

### 仮定

Unityは「スケール調整後、ヒューマノイドポーズの平均ボディ方向は全てのHumanoid Rigで同じ」と仮定しています。

---

## 正規化メートル（Normalized Meters）

### 定義

正規化メートルは、T-Pose時の質量中心高さを **1** として正規化した単位系です。

### スケール（humanScale）

```
humanScale = T-Pose時の質量中心のY座標（地面からの高さ）
```

UnityのAnimator.humanScaleプロパティがこの値を返します。

### 正規化の計算

**ワールド空間 → 正規化空間:**
```
normalizedPosition = worldPosition / humanScale
```

**正規化空間 → ワールド空間:**
```
worldPosition = normalizedPosition × humanScale
```

### 具体例

| キャラクター | 身長 | humanScale（推定） | 説明 |
|------------|-----|-------------------|------|
| 標準成人 | 1.7m | ≈ 0.94m | 質量中心 ≈ 身長の55% |
| 子供 | 1.2m | ≈ 0.66m | |
| 巨人 | 3.0m | ≈ 1.65m | |

全てのキャラクターで、正規化後の質量中心高さは **1.0** になります。

---

## RootT（ルートトランスフォーム）

### 定義

RootTはAnimation Clipに保存されるルートの位置・回転データです。正規化メートルで記録されます。

### Body TransformとRoot Transform

| 項目 | Body Transform | Root Transform |
|-----|---------------|----------------|
| 定義 | 質量中心の位置と平均ボディ方向 | Body TransformのY平面への投影 |
| 用途 | リターゲティングの基準 | 移動（ロコモーション）の基準 |
| 計算 | アニメーション評価時に計算 | Body Transformから導出 |

### Root Transform Position (Y) の計算

```
RootT.y = (質量中心.y - 基準点.y) / humanScale
```

#### 足を接地させる場合

足を地面（Y=0）に接地させたい場合：

```
footY = min(LeftFoot.y, RightFoot.y)
RootT.y = (centerOfMass.y - footY) / humanScale
```

#### 計算例

humanScale = 1.0（質量中心高さ1mのキャラクター）の場合：

| ポーズ | 質量中心.y | 足.y | RootT.y | 説明 |
|-------|-----------|------|---------|------|
| T-Pose直立 | 1.0m | 0m | 1.0 | 標準姿勢 |
| しゃがみ | 0.6m | 0m | 0.6 | 質量中心が下がる |
| 片足上げ | 1.0m | 0m | 1.0 | 接地足を基準 |
| ジャンプ頂点 | 1.5m | 0.5m | 1.0 | 足も上がる |
| 着地 | 0.8m | 0m | 0.8 | 膝を曲げて着地 |

---

## リターゲティング時のスケーリング

### 処理の流れ

1. **RetargetFrom（エディタ時）**
   - ソースアニメーションを正規化
   - 位置データを `sourceHumanScale` で除算
   - Muscle Clipとして保存

2. **RetargetTo（ランタイム時）**
   - Muscle Clipを評価
   - 位置データに `targetHumanScale` を乗算
   - ターゲットキャラクターに適用

### 計算式

```
targetWorldPosition = normalizedPosition × targetHumanScale
                    = (sourceWorldPosition / sourceHumanScale) × targetHumanScale
```

### 具体例

身長1.7mのキャラクター用アニメーションを身長1.2mのキャラクターに適用：

```
sourceHumanScale = 0.94m
targetHumanScale = 0.66m

# 元の質量中心位置が (0, 1.0, 0.5) の場合
normalizedPosition = (0, 1.0, 0.5) / 0.94 = (0, 1.064, 0.532)
targetPosition = (0, 1.064, 0.532) × 0.66 = (0, 0.702, 0.351)
```

---

## プロポーションの違いによる問題

### 発生する問題

スケーリングは身長の違いを補正しますが、**プロポーションの違い**（腕の長さ、脚の長さの比率）は完全に補正できません。

結果として：
- **足滑り（Foot Sliding）**: 歩行時に足が地面を滑る
- **手が届かない**: 握手やオブジェクト操作で位置がずれる
- **自己貫通**: 手が体を貫通する

### 解決策：IKパス

Mecanimは手足の**元のワールド位置・向き**をMuscle Spaceにオプションで保存しています。

```
保存形式:
  handPosition = (worldPosition - rootPosition) / humanScale
  handRotation = worldRotation - rootRotation
```

IKソルバーを使用して、リターゲティング後のポーズを修正：

1. Muscle Spaceから元の手足位置を取得
2. ターゲットのhumanScaleでスケール
3. IKで手足を目標位置に移動

Animation Clipインポート設定の「Foot IK」オプションがこの機能を制御します。

---

## Animation Clip設定

### Root Transform Position (Y)

| 設定 | 説明 |
|-----|------|
| Bake Into Pose | Yの変化をポーズに焼き込む（GameObjectの高さは変わらない） |
| Based Upon | Original / Mass Center (Body) / Feet |
| Offset | 手動オフセット調整 |

### Based Upon オプション

| オプション | 説明 | 用途 |
|-----------|------|------|
| Original | 元のアニメーションの値を使用 | キーフレームアニメーション |
| Mass Center (Body) | 質量中心を基準 | 一般的なモーション |
| Feet | 最も低い足の位置を基準 | 高さが変わるモーション |

「Feet」オプションは、接地点を常に足の位置に合わせるため、ブレンド時の浮遊問題を防ぎます。

---

## 参考情報

### Unityドキュメント

- Animator.humanScale
- Animator.bodyPosition
- HumanPose.bodyPosition
- Root Motion の仕組み

### 学術文献

- de Leva, P. (1996). Adjustments to Zatsiorsky-Seluyanov's segment inertia parameters.
- Dempster, W.T. (1955). Space requirements of the seated operator.

### Unity公式ブログ

- Mecanim Humanoids（2014）

---

---

## Muscle値と角度の変換式

### 確定した計算式

Muscle値から角度への変換：

```
if muscle >= 0:
    angle = center + muscle × max
else:
    angle = center + muscle × (-min)
```

角度からMuscle値への変換：

```
if angle >= center:
    muscle = (angle - center) / max
else:
    muscle = (angle - center) / (-min)
```

### パラメータの意味

| パラメータ | 説明 | 取得方法 |
|-----------|------|---------|
| min | 負方向の最大角度（負の値） | `HumanTrait.GetMuscleDefaultMin(i)` |
| max | 正方向の最大角度（正の値） | `HumanTrait.GetMuscleDefaultMax(i)` |
| center | Muscle=0 の時の角度 | T-Pose muscle値から逆算 |

### center の計算

T-PoseではT-Pose基準の角度 = 0 なので：

```
0 = center + tPoseMuscle × range
center = -tPoseMuscle × range

where range = (tPoseMuscle >= 0) ? max : -min
```

### T-Pose Muscle 値の計算

```
tPoseMuscle = -center / ((center <= 0) ? max : -min)
```

---

## 全Muscleのデフォルト値一覧

### 体幹（Spine/Chest）

| Index | Name | Min | Max | T-Pose Muscle | Center |
|-------|------|-----|-----|---------------|--------|
| 0 | Spine Front-Back | -40 | 40 | 0 | 0 |
| 1 | Spine Left-Right | -40 | 40 | 0 | 0 |
| 2 | Spine Twist Left-Right | -40 | 40 | 0 | 0 |
| 3 | Chest Front-Back | -40 | 40 | 0 | 0 |
| 4 | Chest Left-Right | -40 | 40 | 0 | 0 |
| 5 | Chest Twist Left-Right | -40 | 40 | 0 | 0 |
| 6 | UpperChest Front-Back | -20 | 20 | 0 | 0 |
| 7 | UpperChest Left-Right | -20 | 20 | 0 | 0 |
| 8 | UpperChest Twist Left-Right | -20 | 20 | 0 | 0 |

### 頭部（Neck/Head）

| Index | Name | Min | Max | T-Pose Muscle | Center |
|-------|------|-----|-----|---------------|--------|
| 9 | Neck Nod Down-Up | -40 | 40 | 0 | 0 |
| 10 | Neck Tilt Left-Right | -40 | 40 | 0 | 0 |
| 11 | Neck Turn Left-Right | -40 | 40 | 0 | 0 |
| 12 | Head Nod Down-Up | -40 | 40 | 0 | 0 |
| 13 | Head Tilt Left-Right | -40 | 40 | 0 | 0 |
| 14 | Head Turn Left-Right | -40 | 40 | 0 | 0 |

### 目・顎（Eye/Jaw）

| Index | Name | Min | Max | T-Pose Muscle | Center |
|-------|------|-----|-----|---------------|--------|
| 15 | Left Eye Down-Up | -10 | 15 | 0 | 0 |
| 16 | Left Eye In-Out | -20 | 20 | 0 | 0 |
| 17 | Right Eye Down-Up | -10 | 15 | 0 | 0 |
| 18 | Right Eye In-Out | -20 | 20 | 0 | 0 |
| 19 | Jaw Close | -10 | 10 | 0 | 0 |
| 20 | Jaw Left-Right | -10 | 10 | 0 | 0 |

### 脚（Leg）

| Index | Name | Min | Max | T-Pose Muscle | Center | 備考 |
|-------|------|-----|-----|---------------|--------|------|
| 21 | Left Upper Leg Front-Back | -90 | 50 | 0.6 | -30 | T-Pose = 真下 |
| 22 | Left Upper Leg In-Out | -60 | 60 | 0 | 0 | |
| 23 | Left Upper Leg Twist In-Out | -60 | 60 | 0 | 0 | |
| 24 | Left Lower Leg Stretch | -80 | 80 | 1.0 | -80 | T-Pose = 伸展 |
| 25 | Left Lower Leg Twist In-Out | -90 | 90 | 0 | 0 | |
| 26 | Left Foot Up-Down | -50 | 50 | 0 | 0 | |
| 27 | Left Foot Twist In-Out | -30 | 30 | 0 | 0 | |
| 28 | Left Toes Up-Down | -50 | 50 | 0 | 0 | |
| 29 | Right Upper Leg Front-Back | -90 | 50 | 0.6 | -30 | T-Pose = 真下 |
| 30 | Right Upper Leg In-Out | -60 | 60 | 0 | 0 | |
| 31 | Right Upper Leg Twist In-Out | -60 | 60 | 0 | 0 | |
| 32 | Right Lower Leg Stretch | -80 | 80 | 1.0 | -80 | T-Pose = 伸展 |
| 33 | Right Lower Leg Twist In-Out | -90 | 90 | 0 | 0 | |
| 34 | Right Foot Up-Down | -50 | 50 | 0 | 0 | |
| 35 | Right Foot Twist In-Out | -30 | 30 | 0 | 0 | |
| 36 | Right Toes Up-Down | -50 | 50 | 0 | 0 | |

### 腕（Arm）

| Index | Name | Min | Max | T-Pose Muscle | Center | 備考 |
|-------|------|-----|-----|---------------|--------|------|
| 37 | Left Shoulder Down-Up | -15 | 30 | 0 | 0 | |
| 38 | Left Shoulder Front-Back | -15 | 15 | 0 | 0 | |
| 39 | Left Arm Down-Up | -60 | 100 | 0.4 | -40 | T-Pose = 水平 |
| 40 | Left Arm Front-Back | -100 | 100 | 0.3 | -30 | |
| 41 | Left Arm Twist In-Out | -90 | 90 | 0 | 0 | |
| 42 | Left Forearm Stretch | -80 | 80 | 1.0 | -80 | T-Pose = 伸展 |
| 43 | Left Forearm Twist In-Out | -90 | 90 | 0 | 0 | |
| 44 | Left Hand Down-Up | -80 | 80 | 0 | 0 | |
| 45 | Left Hand In-Out | -40 | 40 | 0 | 0 | |
| 46 | Right Shoulder Down-Up | -15 | 30 | 0 | 0 | |
| 47 | Right Shoulder Front-Back | -15 | 15 | 0 | 0 | |
| 48 | Right Arm Down-Up | -60 | 100 | 0.4 | -40 | T-Pose = 水平 |
| 49 | Right Arm Front-Back | -100 | 100 | 0.3 | -30 | |
| 50 | Right Arm Twist In-Out | -90 | 90 | 0 | 0 | |
| 51 | Right Forearm Stretch | -80 | 80 | 1.0 | -80 | T-Pose = 伸展 |
| 52 | Right Forearm Twist In-Out | -90 | 90 | 0 | 0 | |
| 53 | Right Hand Down-Up | -80 | 80 | 0 | 0 | |
| 54 | Right Hand In-Out | -40 | 40 | 0 | 0 | |

### 指（Finger）- デフォルト値

指のT-Pose Muscle値はモデルの初期ポーズに依存するため、以下はデフォルト範囲のみ記載。

| Index | Name | Min | Max |
|-------|------|-----|-----|
| 55-58 | Left Thumb 1-3 Stretched, Spread | -20~-40 | 20~35 |
| 59-62 | Left Index 1-3 Stretched, Spread | -50~-45 | 50~45 |
| 63-66 | Left Middle 1-3 Stretched, Spread | -50~-45 | 50~45 |
| 67-70 | Left Ring 1-3 Stretched, Spread | -50~-45 | 50~45 |
| 71-74 | Left Little 1-3 Stretched, Spread | -50~-45 | 50~45 |
| 75-94 | Right Fingers | （左と同様） | |

---

## 重要な知見

### T-Pose ≠ Muscle 0

多くのボーンでは T-Pose = Muscle 0 だが、以下のボーンは異なる：

| ボーン | T-Pose Muscle | 理由 |
|-------|---------------|------|
| Upper Leg Front-Back | 0.6 | 非対称な可動範囲（前90°/後50°） |
| Lower Leg Stretch | 1.0 | T-Poseは膝が伸びた状態（max側） |
| Arm Down-Up | 0.4 | 非対称な可動範囲（下60°/上100°） |
| Arm Front-Back | 0.3 | T-Poseで腕がやや後ろ |
| Forearm Stretch | 1.0 | T-Poseは肘が伸びた状態（max側） |

### Stretch Muscle の特性

Lower Leg Stretch, Forearm Stretch は：
- **Muscle = -1**: 最大屈曲（膝/肘を曲げる）
- **Muscle = +1**: 最大伸展（膝/肘を伸ばす）= T-Pose
- **center = -80°**: 可動範囲の min 端に一致

### 計算例

**Upper Leg Front-Back で脚を前に45°上げる場合：**

```
目標角度 = -45°（前方が負）
center = -30°
max = 50°, min = -90°

angle = -45° < center = -30° なので min 側
muscle = (angle - center) / (-min)
       = (-45 - (-30)) / 90
       = -15 / 90
       = -0.167
```

**Lower Leg Stretch で膝を45°曲げる場合：**

```
目標角度 = -45°（屈曲が負）
center = -80°
max = 80°, min = -80°

angle = -45° > center = -80° なので max 側
muscle = (angle - center) / max
       = (-45 - (-80)) / 80
       = 35 / 80
       = 0.4375
```

---

## 更新履歴

- 2026-01-03: Muscle計算式とcenter値を追加
- 2026-01-02: 初版作成
