# Unity Humanoid Muscle軸の方向（T-Pose vs Fetus Pose）

## Muscle軸の定義

| DoF | 軸名 | 意味 |
|-----|------|------|
| 0 | X (Twist) | ボーン伸長方向周りの回転（ひねり） |
| 1 | Y (Spread) | 開脚・開腕方向 |
| 2 | Z (Stretch) | 屈曲・伸展方向（正面に近い軸） |

## T-Pose でのMuscle軸方向（ワールド座標）

正面 = +Z として計算

| Bone | X (Twist) | Y (Spread) | Z (Stretch) |
|------|-----------|------------|-------------|
| **胴体** ||||
| Hips | +Y | +X | +Z |
| Spine | +Y | +X | +Z |
| Chest | +Y | +X | +Z |
| UpperChest | +Y | +X | +Z |
| Neck | +Y | +X | +Z |
| Head | +Y | +X | +Z |
| **左脚** ||||
| LeftUpperLeg | -Y | -X | +Z |
| LeftLowerLeg | -Y | -X | +Z |
| LeftFoot | +Z | -X | +Y |
| **右脚** ||||
| RightUpperLeg | -Y | -X | +Z |
| RightLowerLeg | -Y | -X | +Z |
| RightFoot | +Z | -X | +Y |
| **左腕** ||||
| LeftShoulder | -X | +Y | +Z |
| LeftUpperArm | -X | +Y | +Z |
| LeftLowerArm | -X | +Y | +Z |
| LeftHand | -X | +Y | +Z |
| **右腕** ||||
| RightShoulder | +X | -Y | +Z |
| RightUpperArm | +X | -Y | +Z |
| RightLowerArm | +X | -Y | +Z |
| RightHand | +X | -Y | +Z |

## Fetus Pose でのMuscle軸方向（ワールド座標）

親ボーンからの累積回転を適用後

| Bone | X (Twist) | Y (Spread) | Z (Stretch) |
|------|-----------|------------|-------------|
| **胴体** ||||
| Hips | +Y | +X | +Z |
| Spine | ~+Y (前傾) | ~+X | +Z |
| Chest | ~+Y (前傾) | ~+X | +Z |
| UpperChest | ~-X | ~+Y | +Z |
| Neck | ~-X | ~+Y | +Z |
| Head | -X | +Y | +Z |
| **左脚** ||||
| LeftUpperLeg | ~+X (屈曲) | ~-Y | +Z |
| LeftLowerLeg | ~-X | ~+Y | +Z |
| LeftFoot | ~+Z | ~+Y | ~+X |
| **右脚** ||||
| RightUpperLeg | ~+X (屈曲) | ~-Y | +Z |
| RightLowerLeg | ~-X | ~+Y | +Z |
| RightFoot | ~+Z | ~+Y | ~+X |
| **左腕** ||||
| LeftShoulder | ~-Y | ~-X | +Z |
| LeftUpperArm | (複合) | ~-X | ~+Z |
| LeftLowerArm | ~+Y | ~+X | ~+Z |
| LeftHand | ~+Y | ~+X | ~+Z |
| **右腕** ||||
| RightShoulder | ~+Y | ~+X | +Z |
| RightUpperArm | ~-X | ~+Z | (複合) |
| RightLowerArm | ~-Y | ~-Z | (複合) |
| RightHand | ~+X | ~-Z | (複合) |

## 解釈のポイント

1. **T-Poseでの規則性**
   - 胴体系: Twist = +Y（上方向）、Stretch = +Z（正面）
   - 脚系: Twist = -Y（下方向）、Stretch = +Z（正面＝膝が曲がる方向）
   - 左腕: Twist = -X（左方向）、Stretch = +Z（正面＝肘が曲がる方向）
   - 右腕: Twist = +X（右方向）、Stretch = +Z（正面＝肘が曲がる方向）

2. **Fetus Poseでの変化**
   - 軸はボーンと一緒に回転する
   - 親ボーンの回転が子に累積する
   - `~` = おおよそその方向（完全一致ではない）

3. **Muscle値の意味**
   - 正のStretch (DoF 2) → 屈曲（膝・肘を曲げる、体を前屈）
   - 正のSpread (DoF 1) → 開く（開脚、腕を体から離す）
   - 正のTwist (DoF 0) → 内旋/外旋（ボーン軸周りの回転）
