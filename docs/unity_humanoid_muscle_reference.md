# Unity Humanoid Muscle リファレンス

## 概要

UnityのHumanoidアニメーションシステム（Mecanim）は、スケルトンリグを抽象化された「Muscle Space」に変換することで、異なるキャラクター間でアニメーションをリターゲティングすることを可能にします。

### 基本概念

- **Muscle値**: 正規化された値 [-1, 1] で、ボーンを特定の軸に沿って回転させる
- **Muscle Referential**: 各ボーンに対する前回転（pre rotation）と後回転（post rotation）、範囲、符号で構成
- **DoF (Degree of Freedom)**: 自由度。0=X軸、1=Y軸、2=Z軸

### 重要な注意点

- Muscle値がすべて0の状態は**T-poseではない**（「胎児ポーズ」になる）
- T-poseはAvatarセットアップ時の「Enforce T-Pose」で定義されるリファレンスポーズ
- 一部のボーンは3軸すべてのMuscleを持たない（例：肘はY軸なし）

---

## Muscle値と角度の対応

### 基本的なマッピング

Muscle値は正規化された値 [-1, 1] で、角度範囲 [min, max] にマッピングされます。

| Muscle値 | 対応する角度 | 説明 |
|---------|-------------|------|
| **-1** | min角度 | 負方向の最大回転 |
| **0** | center（静止位置） | デフォルトの静止状態 |
| **+1** | max角度 | 正方向の最大回転 |

### Muscle名と方向の対応

Muscle名は「A-B」形式（例: Front-Back, Down-Up）で命名されており、以下の規則に従います：

| Muscle値 | 対応する方向 |
|---------|-------------|
| **-1** | 最初の単語（A）の方向 |
| **+1** | 2番目の単語（B）の方向 |

**例**:
- "Left Upper Leg **Front-Back**" → -1 = 前(Front)、+1 = 後ろ(Back)
- "Left Arm **Down-Up**" → -1 = 下(Down)、+1 = 上(Up)
- "Head Tilt **Left-Right**" → -1 = 左(Left)、+1 = 右(Right)
- "Left Forearm **Stretch**" → -1 = 曲げる、+1 = 伸ばす
- "Left Arm **Twist In-Out**" → -1 = 内ひねり(In)、+1 = 外ひねり(Out)

> ⚠️ **注意**: この命名規則はUnity公式ドキュメントには明記されていません。実機での検証に基づく推測です。実際のプロジェクトでは、使用前に動作を確認することを推奨します。

### 計算式

```
角度 = center + muscle_value * (muscle_value >= 0 ? max : -min)
```

より一般的な線形補間として：

```
if (muscle_value >= 0):
    角度 = center + muscle_value * max
else:
    角度 = center + muscle_value * (-min)
```

**注意**: Muscle値は [-1, 1] の範囲外に出ることも可能です（オーバーシュート）。範囲はハードリミットではなく、通常の動作範囲を定義しています。

### 具体例

**Left Arm Down-Up** (Index 39) の場合：
- デフォルト範囲: min = -60°, max = 100°

| Muscle値 | 計算 | 結果角度 |
|---------|------|---------|
| -1.0 | 0 + (-1.0) × (-(-60)) = -60 | **-60°**（腕を下げる） |
| -0.5 | 0 + (-0.5) × (-(-60)) = -30 | **-30°** |
| 0.0 | 0 | **0°**（静止位置） |
| +0.5 | 0 + 0.5 × 100 = 50 | **+50°** |
| +1.0 | 0 + 1.0 × 100 = 100 | **+100°**（腕を上げる） |

### HumanLimitの構成要素

Avatarの各Muscleには以下の設定があります：

| プロパティ | 説明 | 範囲 |
|-----------|------|------|
| **min** | 負方向の最大回転角度 | -180° ~ 0° |
| **max** | 正方向の最大回転角度 | 0° ~ 180° |
| **center** | 静止位置のオフセット | - |
| **useDefaultValues** | デフォルト値を使用するか | bool |

---

## Humanoid Bone一覧（54ボーン）

### 必須ボーン（15ボーン）

| ボーン名 | 説明 |
|---------|------|
| Hips | 骨盤（ルート） |
| Spine | 背骨 |
| Head | 頭 |
| LeftUpperLeg | 左太もも |
| LeftLowerLeg | 左すね |
| LeftFoot | 左足 |
| RightUpperLeg | 右太もも |
| RightLowerLeg | 右すね |
| RightFoot | 右足 |
| LeftUpperArm | 左上腕 |
| LeftLowerArm | 左前腕 |
| LeftHand | 左手 |
| RightUpperArm | 右上腕 |
| RightLowerArm | 右前腕 |
| RightHand | 右手 |

### オプションボーン（体幹・頭部）

| ボーン名 | 説明 |
|---------|------|
| Chest | 胸 |
| UpperChest | 上胸 |
| Neck | 首 |
| Jaw | 顎 |
| LeftEye | 左目 |
| RightEye | 右目 |

### オプションボーン（脚・足）

| ボーン名 | 説明 |
|---------|------|
| LeftToes | 左つま先 |
| RightToes | 右つま先 |

### オプションボーン（腕・肩）

| ボーン名 | 説明 |
|---------|------|
| LeftShoulder | 左肩（鎖骨） |
| RightShoulder | 右肩（鎖骨） |

### オプションボーン（左手指）

| ボーン名 | 説明 |
|---------|------|
| LeftThumbProximal | 左親指・第1関節 |
| LeftThumbIntermediate | 左親指・第2関節 |
| LeftThumbDistal | 左親指・第3関節 |
| LeftIndexProximal | 左人差し指・第1関節 |
| LeftIndexIntermediate | 左人差し指・第2関節 |
| LeftIndexDistal | 左人差し指・第3関節 |
| LeftMiddleProximal | 左中指・第1関節 |
| LeftMiddleIntermediate | 左中指・第2関節 |
| LeftMiddleDistal | 左中指・第3関節 |
| LeftRingProximal | 左薬指・第1関節 |
| LeftRingIntermediate | 左薬指・第2関節 |
| LeftRingDistal | 左薬指・第3関節 |
| LeftLittleProximal | 左小指・第1関節 |
| LeftLittleIntermediate | 左小指・第2関節 |
| LeftLittleDistal | 左小指・第3関節 |

### オプションボーン（右手指）

| ボーン名 | 説明 |
|---------|------|
| RightThumbProximal | 右親指・第1関節 |
| RightThumbIntermediate | 右親指・第2関節 |
| RightThumbDistal | 右親指・第3関節 |
| RightIndexProximal | 右人差し指・第1関節 |
| RightIndexIntermediate | 右人差し指・第2関節 |
| RightIndexDistal | 右人差し指・第3関節 |
| RightMiddleProximal | 右中指・第1関節 |
| RightMiddleIntermediate | 右中指・第2関節 |
| RightMiddleDistal | 右中指・第3関節 |
| RightRingProximal | 右薬指・第1関節 |
| RightRingIntermediate | 右薬指・第2関節 |
| RightRingDistal | 右薬指・第3関節 |
| RightLittleProximal | 右小指・第1関節 |
| RightLittleIntermediate | 右小指・第2関節 |
| RightLittleDistal | 右小指・第3関節 |

---

## Muscle一覧（95 Muscles）

### ボディMuscle（0-54）

**凡例**: `Muscle=-1` → min角度、`Muscle=0` → 静止位置(0°)、`Muscle=+1` → max角度

#### 背骨・胸（0-8）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 0 | Spine Front-Back | Spine | Z | -40° | +40° | 前(-1)↔後ろ(+1) |
| 1 | Spine Left-Right | Spine | Y | -40° | +40° | 左(-1)↔右(+1) |
| 2 | Spine Twist Left-Right | Spine | X | -40° | +40° | 左ひねり(-1)↔右ひねり(+1) |
| 3 | Chest Front-Back | Chest | Z | -40° | +40° | 前(-1)↔後ろ(+1) |
| 4 | Chest Left-Right | Chest | Y | -40° | +40° | 左(-1)↔右(+1) |
| 5 | Chest Twist Left-Right | Chest | X | -40° | +40° | 左ひねり(-1)↔右ひねり(+1) |
| 6 | UpperChest Front-Back | UpperChest | Z | -20° | +20° | 前(-1)↔後ろ(+1) |
| 7 | UpperChest Left-Right | UpperChest | Y | -20° | +20° | 左(-1)↔右(+1) |
| 8 | UpperChest Twist Left-Right | UpperChest | X | -20° | +20° | 左ひねり(-1)↔右ひねり(+1) |

#### 首・頭（9-20）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 9 | Neck Nod Down-Up | Neck | Z | -40° | +40° | 下(-1)↔上(+1) |
| 10 | Neck Tilt Left-Right | Neck | Y | -40° | +40° | 左傾き(-1)↔右傾き(+1) |
| 11 | Neck Turn Left-Right | Neck | X | -40° | +40° | 左向き(-1)↔右向き(+1) |
| 12 | Head Nod Down-Up | Head | Z | -40° | +40° | 下(-1)↔上(+1) |
| 13 | Head Tilt Left-Right | Head | Y | -40° | +40° | 左傾き(-1)↔右傾き(+1) |
| 14 | Head Turn Left-Right | Head | X | -40° | +40° | 左向き(-1)↔右向き(+1) |
| 15 | Left Eye Down-Up | LeftEye | Z | -10° | +15° | 下(-1)↔上(+1) |
| 16 | Left Eye In-Out | LeftEye | Y | -20° | +20° | 内側(-1)↔外側(+1) |
| 17 | Right Eye Down-Up | RightEye | Z | -10° | +15° | 下(-1)↔上(+1) |
| 18 | Right Eye In-Out | RightEye | Y | -20° | +20° | 内側(-1)↔外側(+1) |
| 19 | Jaw Close | Jaw | Z | -10° | +10° | 開く(-1)↔閉じる(+1) |
| 20 | Jaw Left-Right | Jaw | Y | -10° | +10° | 左(-1)↔右(+1) |

#### 左脚（21-28）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 21 | Left Upper Leg Front-Back | LeftUpperLeg | Z | -90° | +50° | 前(-1)↔後ろ(+1) |
| 22 | Left Upper Leg In-Out | LeftUpperLeg | Y | -60° | +60° | 内側(-1)↔外側(+1) |
| 23 | Left Upper Leg Twist In-Out | LeftUpperLeg | X | -60° | +60° | 内ひねり(-1)↔外ひねり(+1) |
| 24 | Left Lower Leg Stretch | LeftLowerLeg | Z | -80° | +5° | 曲げる(-1)↔伸ばす(+1) |
| 25 | Left Lower Leg Twist In-Out | LeftLowerLeg | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 26 | Left Foot Up-Down | LeftFoot | Z | -50° | +50° | 下(-1)↔上(+1) |
| 27 | Left Foot Twist In-Out | LeftFoot | X | -30° | +30° | 内ひねり(-1)↔外ひねり(+1) |
| 28 | Left Toes Up-Down | LeftToes | Z | -50° | +50° | 下(-1)↔上(+1) |

#### 右脚（29-36）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 29 | Right Upper Leg Front-Back | RightUpperLeg | Z | -90° | +50° | 前(-1)↔後ろ(+1) |
| 30 | Right Upper Leg In-Out | RightUpperLeg | Y | -60° | +60° | 内側(-1)↔外側(+1) |
| 31 | Right Upper Leg Twist In-Out | RightUpperLeg | X | -60° | +60° | 内ひねり(-1)↔外ひねり(+1) |
| 32 | Right Lower Leg Stretch | RightLowerLeg | Z | -80° | +5° | 曲げる(-1)↔伸ばす(+1) |
| 33 | Right Lower Leg Twist In-Out | RightLowerLeg | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 34 | Right Foot Up-Down | RightFoot | Z | -50° | +50° | 下(-1)↔上(+1) |
| 35 | Right Foot Twist In-Out | RightFoot | X | -30° | +30° | 内ひねり(-1)↔外ひねり(+1) |
| 36 | Right Toes Up-Down | RightToes | Z | -50° | +50° | 下(-1)↔上(+1) |

#### 左腕（37-45）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 37 | Left Shoulder Down-Up | LeftShoulder | Z | -15° | +30° | 下(-1)↔上(+1) |
| 38 | Left Shoulder Front-Back | LeftShoulder | Y | -15° | +15° | 前(-1)↔後ろ(+1) |
| 39 | Left Arm Down-Up | LeftUpperArm | Z | -60° | +100° | 下(-1)↔上(+1) |
| 40 | Left Arm Front-Back | LeftUpperArm | Y | -100° | +100° | 前(-1)↔後ろ(+1) |
| 41 | Left Arm Twist In-Out | LeftUpperArm | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 42 | Left Forearm Stretch | LeftLowerArm | Z | -80° | +5° | 曲げる(-1)↔伸ばす(+1) |
| 43 | Left Forearm Twist In-Out | LeftLowerArm | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 44 | Left Hand Down-Up | LeftHand | Z | -80° | +80° | 下(-1)↔上(+1) |
| 45 | Left Hand In-Out | LeftHand | Y | -40° | +40° | 内(-1)↔外(+1) |

#### 右腕（46-54）

| Index | Muscle名 | ボーン | 軸 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|--------|-----|-----------|-----------|---------|
| 46 | Right Shoulder Down-Up | RightShoulder | Z | -15° | +30° | 下(-1)↔上(+1) |
| 47 | Right Shoulder Front-Back | RightShoulder | Y | -15° | +15° | 前(-1)↔後ろ(+1) |
| 48 | Right Arm Down-Up | RightUpperArm | Z | -60° | +100° | 下(-1)↔上(+1) |
| 49 | Right Arm Front-Back | RightUpperArm | Y | -100° | +100° | 前(-1)↔後ろ(+1) |
| 50 | Right Arm Twist In-Out | RightUpperArm | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 51 | Right Forearm Stretch | RightLowerArm | Z | -80° | +5° | 曲げる(-1)↔伸ばす(+1) |
| 52 | Right Forearm Twist In-Out | RightLowerArm | X | -90° | +90° | 内ひねり(-1)↔外ひねり(+1) |
| 53 | Right Hand Down-Up | RightHand | Z | -80° | +80° | 下(-1)↔上(+1) |
| 54 | Right Hand In-Out | RightHand | Y | -40° | +40° | 内(-1)↔外(+1) |

### 左手指Muscle（55-74）

**指のMuscle動作**:
- **Stretched**: -1 = 曲げる（握る）、+1 = 伸ばす（開く）
- **Spread**: -1 = 閉じる（内側）、+1 = 開く（外側）

#### 左親指（55-58）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 55 | Left Thumb 1 Stretched | -20° | +20° | 曲げ(-1)↔伸ばし(+1) |
| 56 | Left Thumb Spread | -25° | +25° | 閉じ(-1)↔開き(+1) |
| 57 | Left Thumb 2 Stretched | -40° | +35° | 曲げ(-1)↔伸ばし(+1) |
| 58 | Left Thumb 3 Stretched | -40° | +35° | 曲げ(-1)↔伸ばし(+1) |

#### 左人差し指（59-62）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 59 | Left Index 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 60 | Left Index Spread | -20° | +20° | 閉じ(-1)↔開き(+1) |
| 61 | Left Index 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 62 | Left Index 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 左中指（63-66）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 63 | Left Middle 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 64 | Left Middle Spread | -7.5° | +7.5° | 閉じ(-1)↔開き(+1) |
| 65 | Left Middle 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 66 | Left Middle 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 左薬指（67-70）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 67 | Left Ring 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 68 | Left Ring Spread | -7.5° | +7.5° | 閉じ(-1)↔開き(+1) |
| 69 | Left Ring 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 70 | Left Ring 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 左小指（71-74）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 71 | Left Little 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 72 | Left Little Spread | -20° | +20° | 閉じ(-1)↔開き(+1) |
| 73 | Left Little 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 74 | Left Little 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

### 右手指Muscle（75-94）

#### 右親指（75-78）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 75 | Right Thumb 1 Stretched | -20° | +20° | 曲げ(-1)↔伸ばし(+1) |
| 76 | Right Thumb Spread | -25° | +25° | 閉じ(-1)↔開き(+1) |
| 77 | Right Thumb 2 Stretched | -40° | +35° | 曲げ(-1)↔伸ばし(+1) |
| 78 | Right Thumb 3 Stretched | -40° | +35° | 曲げ(-1)↔伸ばし(+1) |

#### 右人差し指（79-82）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 79 | Right Index 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 80 | Right Index Spread | -20° | +20° | 閉じ(-1)↔開き(+1) |
| 81 | Right Index 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 82 | Right Index 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 右中指（83-86）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 83 | Right Middle 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 84 | Right Middle Spread | -7.5° | +7.5° | 閉じ(-1)↔開き(+1) |
| 85 | Right Middle 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 86 | Right Middle 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 右薬指（87-90）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 87 | Right Ring 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 88 | Right Ring Spread | -7.5° | +7.5° | 閉じ(-1)↔開き(+1) |
| 89 | Right Ring 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 90 | Right Ring 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

#### 右小指（91-94）

| Index | Muscle名 | -1時の角度 | +1時の角度 | 動作説明 |
|-------|----------|-----------|-----------|---------|
| 91 | Right Little 1 Stretched | -50° | +50° | 曲げ(-1)↔伸ばし(+1) |
| 92 | Right Little Spread | -20° | +20° | 閉じ(-1)↔開き(+1) |
| 93 | Right Little 2 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |
| 94 | Right Little 3 Stretched | -45° | +45° | 曲げ(-1)↔伸ばし(+1) |

---

## 回転軸の詳細

### 軸の定義

Unityは左手座標系を使用しており、各軸の正の回転方向は「その軸の正の無限遠から原点を見たときに時計回り」です。

| DoF Index | 軸 | 一般的な動き |
|-----------|-----|-------------|
| 0 | X | Twist（ひねり）/ Roll |
| 1 | Y | Side（左右）/ Tilt |
| 2 | Z | Front-Back（前後）/ Stretch / Nod |

### ボーン別の軸の意味

#### 背骨系（Spine, Chest, UpperChest）
- **X軸**: Twist（体幹のひねり）
- **Y軸**: Left-Right（左右への傾き）
- **Z軸**: Front-Back（前後への曲げ）

#### 首・頭（Neck, Head）
- **X軸**: Turn（左右を見る）
- **Y軸**: Tilt（首をかしげる）
- **Z軸**: Nod（うなずき）

#### 上腕（UpperArm）
- **X軸**: Twist In-Out（腕のひねり）
- **Y軸**: Front-Back（前後への振り）
- **Z軸**: Down-Up（上下への振り）

#### 前腕（LowerArm）
- **X軸**: Twist In-Out（前腕のひねり）
- **Z軸**: Stretch（肘の曲げ伸ばし）
- ※Y軸のMuscleはなし

#### 上脚（UpperLeg）
- **X軸**: Twist In-Out（脚のひねり）
- **Y軸**: In-Out（内外への開き）
- **Z軸**: Front-Back（前後への振り）

#### 下脚（LowerLeg）
- **X軸**: Twist In-Out（すねのひねり）
- **Z軸**: Stretch（膝の曲げ伸ばし）
- ※Y軸のMuscleはなし

#### 足（Foot）
- **X軸**: Twist In-Out（足首のひねり）
- **Z軸**: Up-Down（足首の曲げ伸ばし）

#### 手（Hand）
- **Y軸**: In-Out（手首の左右への曲げ）
- **Z軸**: Down-Up（手首の上下への曲げ）

---

## スクリプトからのアクセス

### HumanTraitクラス

```csharp
using UnityEngine;

public class MuscleInfo : MonoBehaviour
{
    void Start()
    {
        // 全Muscle名を取得
        string[] muscleNames = HumanTrait.MuscleName;
        int muscleCount = HumanTrait.MuscleCount; // 95
        
        // 全Muscle名とデフォルト範囲を出力
        for (int i = 0; i < muscleCount; i++)
        {
            float min = HumanTrait.GetMuscleDefaultMin(i);
            float max = HumanTrait.GetMuscleDefaultMax(i);
            Debug.Log($"{i}: {muscleNames[i]} ({min}° ~ {max}°)");
        }
    }
}
```

### MuscleFromBone

```csharp
// 特定のボーンのMuscleインデックスを取得
int boneIndex = (int)HumanBodyBones.LeftUpperArm;

// DoF: 0=X軸, 1=Y軸, 2=Z軸
int muscleX = HumanTrait.MuscleFromBone(boneIndex, 0); // Twist
int muscleY = HumanTrait.MuscleFromBone(boneIndex, 1); // Front-Back
int muscleZ = HumanTrait.MuscleFromBone(boneIndex, 2); // Down-Up

// -1が返される場合、そのDoFにMuscleが存在しない
// 例：肘のY軸は-1を返す
```

### HumanPoseHandler

```csharp
using UnityEngine;

public class PoseControl : MonoBehaviour
{
    private HumanPoseHandler poseHandler;
    private HumanPose pose;
    
    void Start()
    {
        Animator animator = GetComponent<Animator>();
        poseHandler = new HumanPoseHandler(animator.avatar, transform);
        pose = new HumanPose();
    }
    
    void Update()
    {
        // 現在のポーズを取得
        poseHandler.GetHumanPose(ref pose);
        
        // Muscle値を変更（例：頭を左に傾ける）
        // Index 13 = Head Tilt Left-Right
        pose.muscles[13] = 0.5f; // 範囲の半分まで傾ける
        
        // ポーズを適用
        poseHandler.SetHumanPose(ref pose);
    }
}
```

---

## Additional Settings

### Translation DoF

Translation DoF（移動の自由度）を有効にすると、以下のボーンで移動アニメーションが可能になります：
- Chest
- UpperChest
- Neck
- LeftUpperLeg / RightUpperLeg
- LeftShoulder / RightShoulder

**注意**: パフォーマンスコストがあるため、必要な場合のみ有効にしてください。

### Twist設定

Humanoid RigはTwistボーンをサポートしていませんが、MecanimソルバーでTwistの分配率を設定できます：
- **Arm Twist**: 上腕と前腕間のTwist分配
- **Forearm Twist**: 前腕と手の間のTwist分配
- **Upper Leg Twist**: 太ももとすねの間のTwist分配
- **Leg Twist**: すねと足の間のTwist分配

デフォルト値は50%で、皮膚の変形問題を軽減します。

---

## 参考リンク

- [Unity Manual - Configuring the Avatar](https://docs.unity3d.com/Manual/ConfiguringtheAvatar.html)
- [Unity Manual - Muscle Settings](https://docs.unity3d.com/Manual/MuscleDefinitions.html)
- [Unity Blog - Mecanim Humanoids](https://blog.unity.com/engine-platform/mecanim-humanoids)
- [HumanTrait API](https://docs.unity3d.com/ScriptReference/HumanTrait.html)
- [HumanPose API](https://docs.unity3d.com/ScriptReference/HumanPose.html)
- [HumanPoseHandler API](https://docs.unity3d.com/ScriptReference/HumanPoseHandler.html)

---

## 更新履歴

- 2026-01-02: Muscle名と方向の対応規則を追加、全Muscleの方向説明を修正
- 2026-01-01: 初版作成
