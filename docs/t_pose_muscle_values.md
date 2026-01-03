# T-Pose における Muscle 値一覧

## 概要

Unity Humanoid の T-Pose を実現するために必要な各 Muscle の値。

**重要**: Muscle = 0 は「可動範囲の中心 (center)」であり、T-Pose とは限らない。

## 計算式

```
T-Pose Muscle = -center / (center <= 0 ? max : -min)
```

ここで center は Muscle=0 時の T-Pose からの角度。

---

## 全 Muscle 一覧 (95個)

### 体幹 (0-8)

| Index | Name | Min | Max | T-Pose | Center |
|-------|------|-----|-----|--------|--------|
| 0 | Spine Front-Back | -40 | 40 | **0** | 0 |
| 1 | Spine Left-Right | -40 | 40 | **0** | 0 |
| 2 | Spine Twist Left-Right | -40 | 40 | **0** | 0 |
| 3 | Chest Front-Back | -40 | 40 | **0** | 0 |
| 4 | Chest Left-Right | -40 | 40 | **0** | 0 |
| 5 | Chest Twist Left-Right | -40 | 40 | **0** | 0 |
| 6 | UpperChest Front-Back | -20 | 20 | **0** | 0 |
| 7 | UpperChest Left-Right | -20 | 20 | **0** | 0 |
| 8 | UpperChest Twist Left-Right | -20 | 20 | **0** | 0 |

### 頭部 (9-14)

| Index | Name | Min | Max | T-Pose | Center |
|-------|------|-----|-----|--------|--------|
| 9 | Neck Nod Down-Up | -40 | 40 | **0** | 0 |
| 10 | Neck Tilt Left-Right | -40 | 40 | **0** | 0 |
| 11 | Neck Turn Left-Right | -40 | 40 | **0** | 0 |
| 12 | Head Nod Down-Up | -40 | 40 | **0** | 0 |
| 13 | Head Tilt Left-Right | -40 | 40 | **0** | 0 |
| 14 | Head Turn Left-Right | -40 | 40 | **0** | 0 |

### 目・顎 (15-20)

| Index | Name | Min | Max | T-Pose | Center |
|-------|------|-----|-----|--------|--------|
| 15 | Left Eye Down-Up | -10 | 15 | **0** | 0 |
| 16 | Left Eye In-Out | -20 | 20 | **0** | 0 |
| 17 | Right Eye Down-Up | -10 | 15 | **0** | 0 |
| 18 | Right Eye In-Out | -20 | 20 | **0** | 0 |
| 19 | Jaw Close | -10 | 10 | **0** | 0 |
| 20 | Jaw Left-Right | -10 | 10 | **0** | 0 |

### 左脚 (21-28)

| Index | Name | Min | Max | T-Pose | Center | 備考 |
|-------|------|-----|-----|--------|--------|------|
| 21 | Left Upper Leg Front-Back | -90 | 50 | **0.6** | -30 | 脚が真下 |
| 22 | Left Upper Leg In-Out | -60 | 60 | **0** | 0 | |
| 23 | Left Upper Leg Twist In-Out | -60 | 60 | **0** | 0 | |
| 24 | Left Lower Leg Stretch | -80 | 80 | **1.0** | -80 | 膝が伸展 |
| 25 | Left Lower Leg Twist In-Out | -90 | 90 | **0** | 0 | |
| 26 | Left Foot Up-Down | -50 | 50 | **0** | 0 | |
| 27 | Left Foot Twist In-Out | -30 | 30 | **0** | 0 | |
| 28 | Left Toes Up-Down | -50 | 50 | **0** | 0 | |

### 右脚 (29-36)

| Index | Name | Min | Max | T-Pose | Center | 備考 |
|-------|------|-----|-----|--------|--------|------|
| 29 | Right Upper Leg Front-Back | -90 | 50 | **0.6** | -30 | 脚が真下 |
| 30 | Right Upper Leg In-Out | -60 | 60 | **0** | 0 | |
| 31 | Right Upper Leg Twist In-Out | -60 | 60 | **0** | 0 | |
| 32 | Right Lower Leg Stretch | -80 | 80 | **1.0** | -80 | 膝が伸展 |
| 33 | Right Lower Leg Twist In-Out | -90 | 90 | **0** | 0 | |
| 34 | Right Foot Up-Down | -50 | 50 | **0** | 0 | |
| 35 | Right Foot Twist In-Out | -30 | 30 | **0** | 0 | |
| 36 | Right Toes Up-Down | -50 | 50 | **0** | 0 | |

### 左腕 (37-45)

| Index | Name | Min | Max | T-Pose | Center | 備考 |
|-------|------|-----|-----|--------|--------|------|
| 37 | Left Shoulder Down-Up | -15 | 30 | **0** | 0 | |
| 38 | Left Shoulder Front-Back | -15 | 15 | **0** | 0 | |
| 39 | Left Arm Down-Up | -60 | 100 | **0.4** | -40 | 腕が水平 |
| 40 | Left Arm Front-Back | -100 | 100 | **0.3** | -30 | |
| 41 | Left Arm Twist In-Out | -90 | 90 | **0** | 0 | |
| 42 | Left Forearm Stretch | -80 | 80 | **1.0** | -80 | 肘が伸展 |
| 43 | Left Forearm Twist In-Out | -90 | 90 | **0** | 0 | |
| 44 | Left Hand Down-Up | -80 | 80 | **0** | 0 | |
| 45 | Left Hand In-Out | -40 | 40 | **0** | 0 | |

### 右腕 (46-54)

| Index | Name | Min | Max | T-Pose | Center | 備考 |
|-------|------|-----|-----|--------|--------|------|
| 46 | Right Shoulder Down-Up | -15 | 30 | **0** | 0 | |
| 47 | Right Shoulder Front-Back | -15 | 15 | **0** | 0 | |
| 48 | Right Arm Down-Up | -60 | 100 | **0.4** | -40 | 腕が水平 |
| 49 | Right Arm Front-Back | -100 | 100 | **0.3** | -30 | |
| 50 | Right Arm Twist In-Out | -90 | 90 | **0** | 0 | |
| 51 | Right Forearm Stretch | -80 | 80 | **1.0** | -80 | 肘が伸展 |
| 52 | Right Forearm Twist In-Out | -90 | 90 | **0** | 0 | |
| 53 | Right Hand Down-Up | -80 | 80 | **0** | 0 | |
| 54 | Right Hand In-Out | -40 | 40 | **0** | 0 | |

### 左手指 (55-74)

| Index | Name | Min | Max | T-Pose | Center |
|-------|------|-----|-----|--------|--------|
| 55 | Left Thumb 1 Stretched | -20 | 20 | **0** | 0 |
| 56 | Left Thumb Spread | -25 | 25 | **0** | 0 |
| 57 | Left Thumb 2 Stretched | -40 | 35 | **0** | 0 |
| 58 | Left Thumb 3 Stretched | -40 | 35 | **0** | 0 |
| 59 | Left Index 1 Stretched | -50 | 50 | **0** | 0 |
| 60 | Left Index Spread | -20 | 20 | **0** | 0 |
| 61 | Left Index 2 Stretched | -45 | 45 | **0** | 0 |
| 62 | Left Index 3 Stretched | -45 | 45 | **0** | 0 |
| 63 | Left Middle 1 Stretched | -50 | 50 | **0** | 0 |
| 64 | Left Middle Spread | -7.5 | 7.5 | **0** | 0 |
| 65 | Left Middle 2 Stretched | -45 | 45 | **0** | 0 |
| 66 | Left Middle 3 Stretched | -45 | 45 | **0** | 0 |
| 67 | Left Ring 1 Stretched | -50 | 50 | **0** | 0 |
| 68 | Left Ring Spread | -7.5 | 7.5 | **0** | 0 |
| 69 | Left Ring 2 Stretched | -45 | 45 | **0** | 0 |
| 70 | Left Ring 3 Stretched | -45 | 45 | **0** | 0 |
| 71 | Left Little 1 Stretched | -50 | 50 | **0** | 0 |
| 72 | Left Little Spread | -20 | 20 | **0** | 0 |
| 73 | Left Little 2 Stretched | -45 | 45 | **0** | 0 |
| 74 | Left Little 3 Stretched | -45 | 45 | **0** | 0 |

### 右手指 (75-94)

| Index | Name | Min | Max | T-Pose | Center |
|-------|------|-----|-----|--------|--------|
| 75 | Right Thumb 1 Stretched | -20 | 20 | **0** | 0 |
| 76 | Right Thumb Spread | -25 | 25 | **0** | 0 |
| 77 | Right Thumb 2 Stretched | -40 | 35 | **0** | 0 |
| 78 | Right Thumb 3 Stretched | -40 | 35 | **0** | 0 |
| 79 | Right Index 1 Stretched | -50 | 50 | **0** | 0 |
| 80 | Right Index Spread | -20 | 20 | **0** | 0 |
| 81 | Right Index 2 Stretched | -45 | 45 | **0** | 0 |
| 82 | Right Index 3 Stretched | -45 | 45 | **0** | 0 |
| 83 | Right Middle 1 Stretched | -50 | 50 | **0** | 0 |
| 84 | Right Middle Spread | -7.5 | 7.5 | **0** | 0 |
| 85 | Right Middle 2 Stretched | -45 | 45 | **0** | 0 |
| 86 | Right Middle 3 Stretched | -45 | 45 | **0** | 0 |
| 87 | Right Ring 1 Stretched | -50 | 50 | **0** | 0 |
| 88 | Right Ring Spread | -7.5 | 7.5 | **0** | 0 |
| 89 | Right Ring 2 Stretched | -45 | 45 | **0** | 0 |
| 90 | Right Ring 3 Stretched | -45 | 45 | **0** | 0 |
| 91 | Right Little 1 Stretched | -50 | 50 | **0** | 0 |
| 92 | Right Little Spread | -20 | 20 | **0** | 0 |
| 93 | Right Little 2 Stretched | -45 | 45 | **0** | 0 |
| 94 | Right Little 3 Stretched | -45 | 45 | **0** | 0 |

---

## T-Pose ≠ 0 の Muscle (まとめ)

| Index | Name | T-Pose | Center | 理由 |
|-------|------|--------|--------|------|
| 21 | Left Upper Leg Front-Back | **0.6** | -30° | 非対称可動域 (前90°/後50°) |
| 24 | Left Lower Leg Stretch | **1.0** | -80° | T-Pose = 最大伸展 |
| 29 | Right Upper Leg Front-Back | **0.6** | -30° | 非対称可動域 (前90°/後50°) |
| 32 | Right Lower Leg Stretch | **1.0** | -80° | T-Pose = 最大伸展 |
| 39 | Left Arm Down-Up | **0.4** | -40° | 非対称可動域 (下60°/上100°) |
| 40 | Left Arm Front-Back | **0.3** | -30° | T-Pose で腕がやや後方 |
| 42 | Left Forearm Stretch | **1.0** | -80° | T-Pose = 最大伸展 |
| 48 | Right Arm Down-Up | **0.4** | -40° | 非対称可動域 (下60°/上100°) |
| 49 | Right Arm Front-Back | **0.3** | -30° | T-Pose で腕がやや後方 |
| 51 | Right Forearm Stretch | **1.0** | -80° | T-Pose = 最大伸展 |

---

## C# コード例

```csharp
public static float[] GetTPoseMuscles()
{
    float[] muscles = new float[95];
    
    // デフォルトは全て 0
    for (int i = 0; i < 95; i++) muscles[i] = 0f;
    
    // T-Pose ≠ 0 の Muscle を設定
    muscles[21] = 0.6f;  // Left Upper Leg Front-Back
    muscles[24] = 1.0f;  // Left Lower Leg Stretch
    muscles[29] = 0.6f;  // Right Upper Leg Front-Back
    muscles[32] = 1.0f;  // Right Lower Leg Stretch
    muscles[39] = 0.4f;  // Left Arm Down-Up
    muscles[40] = 0.3f;  // Left Arm Front-Back
    muscles[42] = 1.0f;  // Left Forearm Stretch
    muscles[48] = 0.4f;  // Right Arm Down-Up
    muscles[49] = 0.3f;  // Right Arm Front-Back
    muscles[51] = 1.0f;  // Right Forearm Stretch
    
    return muscles;
}
```

---

## 注意事項

1. **指の値はモデル依存**
   - モデルの Bind Pose が T-Pose と異なる場合、指の T-Pose Muscle 値も 0 ではない可能性あり

2. **実測推奨**
   - 正確な値は `HumanPoseHandler.GetHumanPose()` で T-Pose モデルから取得するのが確実

3. **Forearm Stretch の center**
   - 実測では -80° だが、GetMuscleDefaultMin/Max は -80/+80 を返す
   - T-Pose = 1.0 となる特殊なケース
