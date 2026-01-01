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

#### 背骨・胸（0-8）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 0 | Spine Front-Back | Spine | Z | -40° ~ 40° |
| 1 | Spine Left-Right | Spine | Y | -40° ~ 40° |
| 2 | Spine Twist Left-Right | Spine | X | -40° ~ 40° |
| 3 | Chest Front-Back | Chest | Z | -40° ~ 40° |
| 4 | Chest Left-Right | Chest | Y | -40° ~ 40° |
| 5 | Chest Twist Left-Right | Chest | X | -40° ~ 40° |
| 6 | UpperChest Front-Back | UpperChest | Z | -20° ~ 20° |
| 7 | UpperChest Left-Right | UpperChest | Y | -20° ~ 20° |
| 8 | UpperChest Twist Left-Right | UpperChest | X | -20° ~ 20° |

#### 首・頭（9-17）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 9 | Neck Nod Down-Up | Neck | Z | -40° ~ 40° |
| 10 | Neck Tilt Left-Right | Neck | Y | -40° ~ 40° |
| 11 | Neck Turn Left-Right | Neck | X | -40° ~ 40° |
| 12 | Head Nod Down-Up | Head | Z | -40° ~ 40° |
| 13 | Head Tilt Left-Right | Head | Y | -40° ~ 40° |
| 14 | Head Turn Left-Right | Head | X | -40° ~ 40° |
| 15 | Left Eye Down-Up | LeftEye | Z | -10° ~ 15° |
| 16 | Left Eye In-Out | LeftEye | Y | -20° ~ 20° |
| 17 | Right Eye Down-Up | RightEye | Z | -10° ~ 15° |
| 18 | Right Eye In-Out | RightEye | Y | -20° ~ 20° |
| 19 | Jaw Close | Jaw | Z | -10° ~ 10° |
| 20 | Jaw Left-Right | Jaw | Y | -10° ~ 10° |

#### 左脚（21-28）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 21 | Left Upper Leg Front-Back | LeftUpperLeg | Z | -90° ~ 50° |
| 22 | Left Upper Leg In-Out | LeftUpperLeg | Y | -60° ~ 60° |
| 23 | Left Upper Leg Twist In-Out | LeftUpperLeg | X | -60° ~ 60° |
| 24 | Left Lower Leg Stretch | LeftLowerLeg | Z | -80° ~ 5° |
| 25 | Left Lower Leg Twist In-Out | LeftLowerLeg | X | -90° ~ 90° |
| 26 | Left Foot Up-Down | LeftFoot | Z | -50° ~ 50° |
| 27 | Left Foot Twist In-Out | LeftFoot | X | -30° ~ 30° |
| 28 | Left Toes Up-Down | LeftToes | Z | -50° ~ 50° |

#### 右脚（29-36）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 29 | Right Upper Leg Front-Back | RightUpperLeg | Z | -90° ~ 50° |
| 30 | Right Upper Leg In-Out | RightUpperLeg | Y | -60° ~ 60° |
| 31 | Right Upper Leg Twist In-Out | RightUpperLeg | X | -60° ~ 60° |
| 32 | Right Lower Leg Stretch | RightLowerLeg | Z | -80° ~ 5° |
| 33 | Right Lower Leg Twist In-Out | RightLowerLeg | X | -90° ~ 90° |
| 34 | Right Foot Up-Down | RightFoot | Z | -50° ~ 50° |
| 35 | Right Foot Twist In-Out | RightFoot | X | -30° ~ 30° |
| 36 | Right Toes Up-Down | RightToes | Z | -50° ~ 50° |

#### 左腕（37-45）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 37 | Left Shoulder Down-Up | LeftShoulder | Z | -15° ~ 30° |
| 38 | Left Shoulder Front-Back | LeftShoulder | Y | -15° ~ 15° |
| 39 | Left Arm Down-Up | LeftUpperArm | Z | -60° ~ 100° |
| 40 | Left Arm Front-Back | LeftUpperArm | Y | -100° ~ 100° |
| 41 | Left Arm Twist In-Out | LeftUpperArm | X | -90° ~ 90° |
| 42 | Left Forearm Stretch | LeftLowerArm | Z | -80° ~ 5° |
| 43 | Left Forearm Twist In-Out | LeftLowerArm | X | -90° ~ 90° |
| 44 | Left Hand Down-Up | LeftHand | Z | -80° ~ 80° |
| 45 | Left Hand In-Out | LeftHand | Y | -40° ~ 40° |

#### 右腕（46-54）

| Index | Muscle名 | 関連ボーン | 軸 | デフォルト範囲 |
|-------|----------|-----------|-----|---------------|
| 46 | Right Shoulder Down-Up | RightShoulder | Z | -15° ~ 30° |
| 47 | Right Shoulder Front-Back | RightShoulder | Y | -15° ~ 15° |
| 48 | Right Arm Down-Up | RightUpperArm | Z | -60° ~ 100° |
| 49 | Right Arm Front-Back | RightUpperArm | Y | -100° ~ 100° |
| 50 | Right Arm Twist In-Out | RightUpperArm | X | -90° ~ 90° |
| 51 | Right Forearm Stretch | RightLowerArm | Z | -80° ~ 5° |
| 52 | Right Forearm Twist In-Out | RightLowerArm | X | -90° ~ 90° |
| 53 | Right Hand Down-Up | RightHand | Z | -80° ~ 80° |
| 54 | Right Hand In-Out | RightHand | Y | -40° ~ 40° |

### 左手指Muscle（55-74）

#### 左親指（55-58）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 55 | Left Thumb 1 Stretched | -20° ~ 20° |
| 56 | Left Thumb Spread | -25° ~ 25° |
| 57 | Left Thumb 2 Stretched | -40° ~ 35° |
| 58 | Left Thumb 3 Stretched | -40° ~ 35° |

#### 左人差し指（59-62）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 59 | Left Index 1 Stretched | -50° ~ 50° |
| 60 | Left Index Spread | -20° ~ 20° |
| 61 | Left Index 2 Stretched | -45° ~ 45° |
| 62 | Left Index 3 Stretched | -45° ~ 45° |

#### 左中指（63-66）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 63 | Left Middle 1 Stretched | -50° ~ 50° |
| 64 | Left Middle Spread | -7.5° ~ 7.5° |
| 65 | Left Middle 2 Stretched | -45° ~ 45° |
| 66 | Left Middle 3 Stretched | -45° ~ 45° |

#### 左薬指（67-70）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 67 | Left Ring 1 Stretched | -50° ~ 50° |
| 68 | Left Ring Spread | -7.5° ~ 7.5° |
| 69 | Left Ring 2 Stretched | -45° ~ 45° |
| 70 | Left Ring 3 Stretched | -45° ~ 45° |

#### 左小指（71-74）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 71 | Left Little 1 Stretched | -50° ~ 50° |
| 72 | Left Little Spread | -20° ~ 20° |
| 73 | Left Little 2 Stretched | -45° ~ 45° |
| 74 | Left Little 3 Stretched | -45° ~ 45° |

### 右手指Muscle（75-94）

#### 右親指（75-78）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 75 | Right Thumb 1 Stretched | -20° ~ 20° |
| 76 | Right Thumb Spread | -25° ~ 25° |
| 77 | Right Thumb 2 Stretched | -40° ~ 35° |
| 78 | Right Thumb 3 Stretched | -40° ~ 35° |

#### 右人差し指（79-82）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 79 | Right Index 1 Stretched | -50° ~ 50° |
| 80 | Right Index Spread | -20° ~ 20° |
| 81 | Right Index 2 Stretched | -45° ~ 45° |
| 82 | Right Index 3 Stretched | -45° ~ 45° |

#### 右中指（83-86）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 83 | Right Middle 1 Stretched | -50° ~ 50° |
| 84 | Right Middle Spread | -7.5° ~ 7.5° |
| 85 | Right Middle 2 Stretched | -45° ~ 45° |
| 86 | Right Middle 3 Stretched | -45° ~ 45° |

#### 右薬指（87-90）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 87 | Right Ring 1 Stretched | -50° ~ 50° |
| 88 | Right Ring Spread | -7.5° ~ 7.5° |
| 89 | Right Ring 2 Stretched | -45° ~ 45° |
| 90 | Right Ring 3 Stretched | -45° ~ 45° |

#### 右小指（91-94）

| Index | Muscle名 | デフォルト範囲 |
|-------|----------|---------------|
| 91 | Right Little 1 Stretched | -50° ~ 50° |
| 92 | Right Little Spread | -20° ~ 20° |
| 93 | Right Little 2 Stretched | -45° ~ 45° |
| 94 | Right Little 3 Stretched | -45° ~ 45° |

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

- 2026-01-01: 初版作成
