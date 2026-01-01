# Humanoid Animation Generator 仕様書

## 概要

Unity Humanoid rig 対応のアニメーションクリップを生成するCLIツール。
固定ボーン（複数）と動作ボーン（エフェクター、1つ）を指定し、IKによって自然な動きを生成する。

## 開発環境

- 言語: Haskell (GHC 9.4+)
- ビルドツール: Cabal または Stack
- 主要ライブラリ:
  - `linear`: ベクトル・クォータニオン演算
  - `aeson`: JSON パース
  - `yaml`: YAML パース
  - `bytestring`: バイナリ出力
  - `containers`: Map, Set
  - `optparse-applicative`: CLI引数パース
  - `gloss` (オプション): 簡易プレビュー

---

## プロジェクト構造

```
humanoid-anim/
├── app/
│   └── Main.hs                 # CLI エントリーポイント
├── src/
│   ├── HumanoidAnim/
│   │   ├── Types.hs            # 共通型定義
│   │   ├── Skeleton/
│   │   │   ├── Bones.hs        # ボーン定義
│   │   │   ├── Hierarchy.hs    # 親子関係
│   │   │   ├── Config.hs       # スケルトン設定
│   │   │   └── Transform.hs    # 回転計算
│   │   ├── IK/
│   │   │   ├── Core.hs         # IK型クラス・共通型
│   │   │   ├── FABRIK.hs       # FABRIKソルバー
│   │   │   ├── CCD.hs          # CCDソルバー（後で実装）
│   │   │   └── Hybrid.hs       # ハイブリッドソルバー（後で実装）
│   │   ├── Motion/
│   │   │   ├── Trajectory.hs   # 軌道定義
│   │   │   ├── Keyframe.hs     # キーフレーム処理
│   │   │   ├── Interpolation.hs # 補間
│   │   │   └── Easing.hs       # イージング関数
│   │   ├── Input/
│   │   │   ├── Config.hs       # 設定ファイルパーサー
│   │   │   ├── Blender.hs      # Blenderエクスポート読み込み
│   │   │   └── Validation.hs   # 入力検証
│   │   ├── Output/
│   │   │   ├── GLTF.hs         # glTF/glb出力
│   │   │   └── UnityAnim.hs    # .anim出力（Phase2）
│   │   ├── Animation.hs        # アニメーション生成メイン
│   │   └── Error.hs            # エラー・警告処理
│   └── HumanoidAnim.hs         # 公開API
├── test/
│   ├── Spec.hs
│   ├── IK/
│   │   └── FABRIKSpec.hs
│   └── Motion/
│       └── InterpolationSpec.hs
├── examples/
│   ├── wave_hand.yaml
│   ├── reach_forward.yaml
│   └── blender_export.json
├── humanoid-anim.cabal
├── README.md
└── SPECIFICATION.md            # この仕様書
```

---

## 型定義

### 基本型

```haskell
-- src/HumanoidAnim/Types.hs

module HumanoidAnim.Types where

import Linear (V3(..), Quaternion(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | 3D位置（メートル単位）
type Position = V3 Float

-- | 回転（クォータニオン）
type Rotation = Quaternion Float

-- | トランスフォーム
data Transform = Transform
  { transformPosition :: !Position
  , transformRotation :: !Rotation
  } deriving (Show, Eq)

-- | ポーズ（全ボーンの位置）
type Pose = Map.Map HumanoidBone Position

-- | フルポーズ（位置と回転）
type FullPose = Map.Map HumanoidBone Transform

-- | 軌道関数（正規化時間 0-1 → 位置）
type Trajectory = Float -> Position

-- | 時間（秒）
type Time = Float

-- | フレーム番号
type FrameNumber = Int
```

### ボーン定義

```haskell
-- src/HumanoidAnim/Skeleton/Bones.hs

module HumanoidAnim.Skeleton.Bones where

-- | Humanoidボーン（全ボーン定義、有効/無効は設定で制御）
data HumanoidBone
  -- 必須ボーン (15)
  = Hips
  | Spine | Chest | Neck | Head
  | LeftUpperArm | LeftLowerArm | LeftHand
  | RightUpperArm | RightLowerArm | RightHand
  | LeftUpperLeg | LeftLowerLeg | LeftFoot
  | RightUpperLeg | RightLowerLeg | RightFoot
  
  -- 推奨ボーン (追加6 = 計21)
  | UpperChest
  | LeftShoulder | RightShoulder
  | LeftToes | RightToes
  | Jaw
  
  -- フルボーン: 指 (追加30)
  | LeftThumbProximal | LeftThumbIntermediate | LeftThumbDistal
  | LeftIndexProximal | LeftIndexIntermediate | LeftIndexDistal
  | LeftMiddleProximal | LeftMiddleIntermediate | LeftMiddleDistal
  | LeftRingProximal | LeftRingIntermediate | LeftRingDistal
  | LeftLittleProximal | LeftLittleIntermediate | LeftLittleDistal
  | RightThumbProximal | RightThumbIntermediate | RightThumbDistal
  | RightIndexProximal | RightIndexIntermediate | RightIndexDistal
  | RightMiddleProximal | RightMiddleIntermediate | RightMiddleDistal
  | RightRingProximal | RightRingIntermediate | RightRingDistal
  | RightLittleProximal | RightLittleIntermediate | RightLittleDistal
  
  -- フルボーン: その他 (追加4)
  | LeftEye | RightEye
  | UpperChestTwist | SpineTwist
  
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

-- | ボーンカテゴリ
data BoneCategory = Required | Recommended | Optional
  deriving (Show, Eq, Ord)

-- | スケルトン詳細度
data SkeletonDetail = MinimalSkeleton | StandardSkeleton | FullSkeleton
  deriving (Show, Eq, Ord, Read)

-- | ボーンのカテゴリ判定
boneCategory :: HumanoidBone -> BoneCategory

-- | 指定カテゴリまでのボーンリスト
bonesUpTo :: BoneCategory -> [HumanoidBone]
```

### スケルトン

```haskell
-- src/HumanoidAnim/Skeleton/Config.hs

module HumanoidAnim.Skeleton.Config where

-- | スケルトン設定
data SkeletonConfig = SkeletonConfig
  { skeletonDetail      :: !SkeletonDetail      -- 詳細度
  , customBoneLengths   :: !(Map HumanoidBone Float)  -- カスタム長さ
  , enabledBones        :: !(Maybe (Set HumanoidBone)) -- カスタム有効ボーン
  } deriving (Show, Eq)

-- | スケルトン
data Skeleton = Skeleton
  { skeletonParents     :: !(Map HumanoidBone (Maybe HumanoidBone))
  , skeletonLengths     :: !(Map HumanoidBone Float)
  , skeletonRestPose    :: !FullPose  -- Tポーズ
  , skeletonActiveBones :: !(Set HumanoidBone)
  } deriving (Show, Eq)

-- | デフォルト設定（StandardSkeleton）
defaultSkeletonConfig :: SkeletonConfig

-- | 設定からスケルトン構築
buildSkeleton :: SkeletonConfig -> Skeleton
```

### IK

```haskell
-- src/HumanoidAnim/IK/Core.hs

module HumanoidAnim.IK.Core where

-- | IK制約
data IKConstraint
  = Fixed HumanoidBone Position              -- 固定ボーン
  | Effector HumanoidBone Position           -- エフェクター（位置のみ）
  | EffectorWithRotation HumanoidBone Position Rotation  -- 位置+回転
  deriving (Show, Eq)

-- | IK入力
data IKInput = IKInput
  { ikSkeleton    :: !Skeleton
  , ikInitialPose :: !Pose
  , ikConstraints :: ![IKConstraint]
  } deriving (Show)

-- | IK出力
data IKOutput = IKOutput
  { ikResultPose   :: !Pose
  , ikIterations   :: !Int
  , ikConverged    :: !Bool
  , ikError        :: !Float
  , ikWarnings     :: ![IKWarning]
  } deriving (Show)

-- | IK警告
data IKWarning
  = UnreachableTarget HumanoidBone Float
  | DidNotConverge Int Float
  deriving (Show, Eq)

-- | IKソルバー型クラス
class IKSolver a where
  type SolverConfig a
  defaultSolverConfig :: SolverConfig a
  solve :: SolverConfig a -> IKInput -> IKOutput
  solverName :: a -> String
```

### モーション

```haskell
-- src/HumanoidAnim/Motion/Keyframe.hs

module HumanoidAnim.Motion.Keyframe where

-- | イージング
data Easing
  = Linear
  | EaseIn | EaseOut | EaseInOut
  | Cubic | CubicIn | CubicOut
  | Bounce
  | Custom (Float -> Float)

instance Show Easing where
  show (Custom _) = "Custom"
  show e = -- 標準のshow

-- | キーフレーム
data Keyframe = Keyframe
  { keyTime     :: !Time        -- 秒
  , keyPosition :: !Position
  , keyRotation :: !(Maybe Rotation)  -- 省略可
  , keyEasing   :: !Easing      -- デフォルト: Linear
  } deriving (Show)

-- | キーフレーム列から軌道生成
keyframesToTrajectory :: [Keyframe] -> Trajectory
```

### アニメーション

```haskell
-- src/HumanoidAnim/Animation.hs

module HumanoidAnim.Animation where

-- | ループモード
data LoopMode = Once | Cycle | PingPong
  deriving (Show, Eq, Read)

-- | アニメーションフレーム
data AnimationFrame = AnimationFrame
  { frameTime      :: !Time
  , framePose      :: !FullPose
  } deriving (Show)

-- | アニメーションクリップ
data AnimationClip = AnimationClip
  { clipName       :: !String
  , clipDuration   :: !Time
  , clipFrameRate  :: !Float
  , clipFrames     :: ![AnimationFrame]
  , clipLoopMode   :: !LoopMode
  } deriving (Show)

-- | アニメーション生成設定
data GenerationConfig = GenerationConfig
  { genFrameRate   :: !Float        -- フレームレート
  , genFrameCount  :: !(Maybe Int)  -- 分割フレーム数（指定時はこちら優先）
  , genSolver      :: !SomeSolver   -- IKソルバー
  , genOptimize    :: !Bool         -- キーフレーム最適化
  } deriving (Show)

-- | アニメーション生成
generateAnimation 
  :: GenerationConfig
  -> Skeleton
  -> [IKConstraint]      -- 固定制約
  -> HumanoidBone        -- エフェクターボーン
  -> [Keyframe]          -- エフェクター軌道
  -> LoopMode
  -> Either GenerationError AnimationClip
```

---

## 設定ファイル仕様

### YAML形式

```yaml
# 必須フィールド
name: "AnimationName"           # アニメーション名
duration: 2.0                   # 秒

fixed:                          # 固定ボーン（1つ以上必須）
  - bone: "Hips"
    position: [0, 1, 0]         # [x, y, z] メートル
  - bone: "LeftFoot"
    position: [-0.1, 0, 0]
  - bone: "RightFoot"
    position: [0.1, 0, 0]

effector:                       # エフェクター（必須）
  bone: "RightHand"
  keyframes:                    # 2つ以上必須
    - time: 0.0
      position: [0.3, 1.0, 0.2]
      easing: "easeInOut"       # 省略可、デフォルト: linear
    - time: 1.0
      position: [0.5, 1.4, 0.4]
      rotation: [0, 0, 0, 1]    # 省略可 [x, y, z, w]
    - time: 2.0
      position: [0.3, 1.0, 0.2]

# 省略可能フィールド（デフォルト値）
config:
  frameRate: 30                 # フレームレート
  frameCount: null              # 分割フレーム数（nullでframeRateから計算）
  solver: "fabrik"              # IKソルバー: fabrik | ccd | hybrid
  loop: "once"                  # ループ: once | cycle | pingpong
  rootMotion: "fixed"           # ルートモーション: fixed | free | y-only
  
skeleton:
  detail: "standard"            # minimal | standard | full
  boneLengths:                  # カスタムボーン長さ（省略可）
    UpperArm: 0.28

output:
  format: "glb"                 # glb | gltf
  optimize: true                # キーフレーム最適化
  precision: 6                  # 小数点精度

# 入力座標系（Blender連携時）
input:
  coordinateSystem: "y-up"      # y-up | z-up（z-upは自動変換）
```

### JSON形式（Blenderエクスポート用）

```json
{
  "frameRate": 30,
  "duration": 2.0,
  "coordinateSystem": "z-up",
  "effector": {
    "bone": "hand.R",
    "keyframes": [
      {"time": 0.0, "position": [0.3, 0.2, 1.0]},
      {"time": 2.0, "position": [0.5, 0.4, 1.4]}
    ]
  },
  "fixedBones": {
    "hips": [0, 0, 1],
    "foot.L": [-0.1, 0, 0],
    "foot.R": [0.1, 0, 0]
  }
}
```

---

## IKアルゴリズム

### FABRIK (Forward And Backward Reaching IK)

```haskell
-- src/HumanoidAnim/IK/FABRIK.hs

module HumanoidAnim.IK.FABRIK where

data FABRIK = FABRIK

data FABRIKConfig = FABRIKConfig
  { fabrikMaxIterations :: !Int     -- デフォルト: 50
  , fabrikTolerance     :: !Float   -- デフォルト: 0.001
  } deriving (Show, Eq)

instance IKSolver FABRIK where
  type SolverConfig FABRIK = FABRIKConfig
  defaultSolverConfig = FABRIKConfig 50 0.001
  solve = solveFABRIK
  solverName _ = "FABRIK"

-- | FABRIKソルバー実装
solveFABRIK :: FABRIKConfig -> IKInput -> IKOutput

-- | Forward pass: エフェクター→ルート
forwardPass :: Position -> [Float] -> [Position] -> [Position]

-- | Backward pass: ルート→エフェクター
backwardPass :: Position -> [Float] -> [Position] -> [Position]
```

### 複数固定ボーン対応

```haskell
-- 複数の固定制約がある場合の処理
-- 1. 各エフェクターについて最も近い固定ボーンを見つける
-- 2. その間のチェーンでFABRIKを実行
-- 3. 固定制約を再適用
-- 4. 収束するまで繰り返し

solveMultiConstraint :: IKInput -> IKOutput
```

---

## 回転計算

### 方式

| ボーンタイプ | 回転計算方式 |
|-------------|-------------|
| 中間ボーン | LookAt（子ボーン方向を向く） |
| 末端ボーン（制約なし） | 親の回転を継承 |
| 末端ボーン（回転指定あり） | 指定された回転を使用 |

```haskell
-- src/HumanoidAnim/Skeleton/Transform.hs

module HumanoidAnim.Skeleton.Transform where

-- | 回転モード
data RotationMode
  = LookAtChild
  | InheritParent
  | FixedRotation Rotation
  deriving (Show)

-- | ボーンの回転モード決定
getRotationMode :: Skeleton -> HumanoidBone -> Maybe Rotation -> RotationMode
getRotationMode skel bone maybeRot
  | Just rot <- maybeRot = FixedRotation rot
  | isTerminalBone skel bone = InheritParent
  | otherwise = LookAtChild

-- | 全ボーンの回転計算
computeRotations :: Skeleton -> Pose -> Map HumanoidBone (Maybe Rotation) -> FullPose

-- | LookAt回転計算
lookRotation :: V3 Float -> V3 Float -> Quaternion Float
```

---

## 出力形式

### glTF/glb

```haskell
-- src/HumanoidAnim/Output/GLTF.hs

module HumanoidAnim.Output.GLTF where

-- | glTF出力オプション
data GLTFOptions = GLTFOptions
  { gltfFormat    :: !GLTFFormat    -- GLB | GLTF
  , gltfPrecision :: !Int           -- 小数点精度
  , gltfOptimize  :: !Bool          -- キーフレーム最適化
  } deriving (Show, Eq)

data GLTFFormat = GLB | GLTF
  deriving (Show, Eq, Read)

-- | アニメーションをglTF出力
exportGLTF :: GLTFOptions -> AnimationClip -> Skeleton -> ByteString

-- | ファイルに書き出し
writeGLTF :: FilePath -> GLTFOptions -> AnimationClip -> Skeleton -> IO ()
```

### glTF構造

```
glTF
├── asset
├── nodes[]           # スケルトンのボーン階層
├── skins[]           # スキニング情報
├── animations[]
│   └── animation
│       ├── name
│       ├── channels[]
│       │   └── channel
│       │       ├── sampler (index)
│       │       └── target
│       │           ├── node (bone index)
│       │           └── path ("translation" | "rotation")
│       └── samplers[]
│           └── sampler
│               ├── input (time accessor)
│               ├── output (value accessor)
│               └── interpolation ("LINEAR")
└── accessors[]       # 時間・位置・回転データ
```

---

## CLIインターフェース

### コマンド

```bash
# メインコマンド
humanoid-anim <command> [options]

# サブコマンド
generate    アニメーション生成
validate    設定ファイル検証
convert     形式変換（Blender JSON → YAML）
info        スケルトン情報表示
```

### generate

```bash
humanoid-anim generate [options]

必須:
  -i, --input FILE      入力設定ファイル (.yaml | .json)
  -o, --output FILE     出力ファイル (.glb | .gltf)

オプション:
  -f, --format FORMAT   出力形式 (glb | gltf) [default: glb]
  --solver SOLVER       IKソルバー (fabrik | ccd) [default: fabrik]
  --fps N               フレームレート [default: 30]
  --frames N            分割フレーム数（fps指定より優先）
  --loop MODE           ループモード (once | cycle | pingpong) [default: once]
  --no-optimize         キーフレーム最適化を無効化
  -v, --verbose         詳細ログ出力
  -q, --quiet           エラーのみ出力
  --strict              警告をエラーとして扱う

例:
  humanoid-anim generate -i wave.yaml -o wave.glb
  humanoid-anim generate -i motion.yaml -o anim.glb --fps 60
  humanoid-anim generate -i motion.yaml -o anim.glb --frames 120
```

### validate

```bash
humanoid-anim validate [options]

必須:
  -i, --input FILE      検証する設定ファイル

オプション:
  --strict              厳格モード（警告も表示）

例:
  humanoid-anim validate -i wave.yaml
```

### convert

```bash
humanoid-anim convert [options]

必須:
  -i, --input FILE      入力ファイル (Blender JSON)
  -o, --output FILE     出力ファイル (.yaml)

例:
  humanoid-anim convert -i blender_export.json -o motion.yaml
```

### info

```bash
humanoid-anim info [options]

オプション:
  --skeleton DETAIL     スケルトン詳細度 (minimal | standard | full)
  --bones               ボーン一覧表示
  --hierarchy           階層構造表示

例:
  humanoid-anim info --skeleton standard --hierarchy
```

---

## エラー処理

### エラーコード

| コード | 説明 |
|--------|------|
| E001 | 設定ファイル読み込みエラー |
| E002 | 必須フィールド不足 |
| E003 | 無効なボーン名 |
| E004 | キーフレーム不足（2つ未満） |
| E005 | 時間順序エラー |
| E006 | 出力ファイル書き込みエラー |

### 警告コード

| コード | 説明 |
|--------|------|
| W001 | 到達不可能な目標位置 |
| W002 | IK収束しなかった |
| W003 | 座標系変換適用 |
| W004 | 不明なフィールド無視 |

```haskell
-- src/HumanoidAnim/Error.hs

module HumanoidAnim.Error where

data Severity = Info | Warning | Error
  deriving (Show, Eq, Ord)

data AppError
  = ConfigReadError FilePath String
  | MissingField String
  | InvalidBoneName String
  | InsufficientKeyframes Int
  | TimeOrderError Time Time
  | OutputWriteError FilePath String
  deriving (Show, Eq)

data AppWarning
  = UnreachableTarget HumanoidBone Float
  | IKNotConverged Int Float
  | CoordinateSystemConverted String String
  | UnknownFieldIgnored String
  deriving (Show, Eq)

data Result a = Result
  { resultValue    :: Either AppError a
  , resultWarnings :: [AppWarning]
  }

-- | 結果の結合
instance Semigroup (Result a) where
instance Monoid (Result ()) where
```

---

## 座標系

### 内部座標系

- **Up軸**: Y-up
- **Forward軸**: Z-forward
- **右手系**
- **単位**: メートル

### 座標変換

```haskell
-- Z-up (Blender) → Y-up (Unity/glTF)
convertZUpToYUp :: V3 Float -> V3 Float
convertZUpToYUp (V3 x y z) = V3 x z (-y)

-- Y-up → Z-up
convertYUpToZUp :: V3 Float -> V3 Float
convertYUpToZUp (V3 x y z) = V3 x (-z) y
```

---

## レストポーズ（Tポーズ）

### 標準ボーン位置（メートル）

```haskell
-- 身長約1.7mの標準体型
defaultRestPositions :: Map HumanoidBone Position
defaultRestPositions = Map.fromList
  [ (Hips,          V3 0.00  1.00  0.00)
  , (Spine,         V3 0.00  1.10  0.00)
  , (Chest,         V3 0.00  1.25  0.00)
  , (UpperChest,    V3 0.00  1.35  0.00)
  , (Neck,          V3 0.00  1.45  0.00)
  , (Head,          V3 0.00  1.55  0.00)
  
  , (LeftShoulder,  V3 0.05  1.40  0.00)
  , (LeftUpperArm,  V3 0.18  1.40  0.00)
  , (LeftLowerArm,  V3 0.46  1.40  0.00)
  , (LeftHand,      V3 0.71  1.40  0.00)
  
  , (RightShoulder, V3 (-0.05) 1.40  0.00)
  , (RightUpperArm, V3 (-0.18) 1.40  0.00)
  , (RightLowerArm, V3 (-0.46) 1.40  0.00)
  , (RightHand,     V3 (-0.71) 1.40  0.00)
  
  , (LeftUpperLeg,  V3 0.10  0.95  0.00)
  , (LeftLowerLeg,  V3 0.10  0.50  0.00)
  , (LeftFoot,      V3 0.10  0.05  0.00)
  , (LeftToes,      V3 0.10  0.00  0.10)
  
  , (RightUpperLeg, V3 (-0.10) 0.95  0.00)
  , (RightLowerLeg, V3 (-0.10) 0.50  0.00)
  , (RightFoot,     V3 (-0.10) 0.05  0.00)
  , (RightToes,     V3 (-0.10) 0.00  0.10)
  ]
```

### 標準ボーン長さ（メートル）

```haskell
defaultBoneLengths :: Map HumanoidBone Float
defaultBoneLengths = Map.fromList
  [ (Spine,         0.10)
  , (Chest,         0.15)
  , (UpperChest,    0.10)
  , (Neck,          0.10)
  , (Head,          0.15)
  
  , (LeftShoulder,  0.13)
  , (LeftUpperArm,  0.28)
  , (LeftLowerArm,  0.25)
  , (LeftHand,      0.10)
  
  , (RightShoulder, 0.13)
  , (RightUpperArm, 0.28)
  , (RightLowerArm, 0.25)
  , (RightHand,     0.10)
  
  , (LeftUpperLeg,  0.45)
  , (LeftLowerLeg,  0.45)
  , (LeftFoot,      0.15)
  , (LeftToes,      0.05)
  
  , (RightUpperLeg, 0.45)
  , (RightLowerLeg, 0.45)
  , (RightFoot,     0.15)
  , (RightToes,     0.05)
  ]
```

---

## 開発フェーズ

### Phase 1（MVP）

- [x] 仕様策定
- [ ] プロジェクトセットアップ
- [ ] 基本型定義
- [ ] スケルトン定義（Standard）
- [ ] FABRIKソルバー
- [ ] キーフレーム処理
- [ ] YAML設定パーサー
- [ ] glb出力
- [ ] CLI（generate, validate）
- [ ] 基本テスト

### Phase 2

- [ ] .anim出力（Unity形式）
- [ ] Blender JSONインポート
- [ ] CCDソルバー
- [ ] 角度制限（膝・肘）
- [ ] CLI（convert）
- [ ] プレビュー機能

### Phase 3

- [ ] フルスケルトン対応
- [ ] ハイブリッドソルバー
- [ ] 最適化ソルバー
- [ ] 複数クリップ対応
- [ ] ルートモーション拡張

---

## テスト方針

### 単体テスト

```haskell
-- test/IK/FABRIKSpec.hs

describe "FABRIK" $ do
  it "converges for reachable target" $ do
    let result = solve defaultSolverConfig input
    ikConverged result `shouldBe` True
    ikError result `shouldSatisfy` (< 0.001)
  
  it "handles unreachable target gracefully" $ do
    let result = solve defaultSolverConfig unreachableInput
    ikConverged result `shouldBe` False
    length (ikWarnings result) `shouldBe` 1

-- test/Motion/InterpolationSpec.hs

describe "Keyframe interpolation" $ do
  it "returns exact position at keyframe time" $ do
    let traj = keyframesToTrajectory keyframes
    traj 0.0 `shouldBe` position1
    traj 1.0 `shouldBe` position2
  
  it "interpolates linearly between keyframes" $ do
    let traj = keyframesToTrajectory linearKeyframes
    traj 0.5 `shouldBe` midpoint
```

### プロパティテスト

```haskell
-- QuickCheck

prop_fabrikPreservesBoneLengths :: Skeleton -> IKInput -> Bool
prop_fabrikPreservesBoneLengths skel input =
  let result = solve defaultSolverConfig input
      originalLengths = computeLengths (ikInitialPose input)
      resultLengths = computeLengths (ikResultPose result)
  in all (uncurry (~=)) (zip originalLengths resultLengths)

prop_interpolationInRange :: [Keyframe] -> Float -> Bool
prop_interpolationInRange kfs t =
  let traj = keyframesToTrajectory kfs
      pos = traj (clamp 0 1 t)
  in inBoundingBox pos (boundingBox kfs)
```

---

## 使用例

### 基本的な使用法

```yaml
# wave_hand.yaml
name: "WaveHand"
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
      position: [0.4, 1.2, 0.2]
      easing: "easeInOut"
    - time: 0.5
      position: [0.5, 1.5, 0.3]
      easing: "easeInOut"
    - time: 1.0
      position: [0.4, 1.2, 0.2]
      easing: "easeInOut"
    - time: 1.5
      position: [0.5, 1.5, 0.3]
      easing: "easeInOut"
    - time: 2.0
      position: [0.4, 1.2, 0.2]

config:
  loop: "cycle"
```

```bash
# 生成
humanoid-anim generate -i wave_hand.yaml -o wave_hand.glb

# フレーム数指定
humanoid-anim generate -i wave_hand.yaml -o wave_hand.glb --frames 60

# ループアニメーション、60fps
humanoid-anim generate -i wave_hand.yaml -o wave_hand.glb --fps 60 --loop cycle
```

### Blenderワークフロー

```bash
# 1. Blenderでポーズ作成・キーフレーム設定
# 2. Pythonスクリプトでエクスポート（blender_export.json）
# 3. 変換
humanoid-anim convert -i blender_export.json -o motion.yaml

# 4. 必要に応じてYAML編集
# 5. 生成
humanoid-anim generate -i motion.yaml -o animation.glb
```

---

## 補足

### Blenderエクスポートスクリプト

```python
# export_keyframes.py
import bpy
import json

def export_effector_keyframes(armature_name, effector_bone, fixed_bones, output_path):
    """
    armature_name: アーマチュア名
    effector_bone: エフェクターボーン名（例: "hand.R"）
    fixed_bones: 固定ボーン名リスト（例: ["hips", "foot.L", "foot.R"]）
    output_path: 出力JSONパス
    """
    obj = bpy.data.objects[armature_name]
    action = obj.animation_data.action
    
    # フレーム範囲
    start_frame, end_frame = action.frame_range
    fps = bpy.context.scene.render.fps
    
    # エフェクターのキーフレーム取得
    bone = obj.pose.bones[effector_bone]
    keyframes = []
    
    # キーフレームのある時間を収集
    fcurves = [fc for fc in action.fcurves if effector_bone in fc.data_path]
    times = sorted(set(kp.co[0] for fc in fcurves for kp in fc.keyframe_points))
    
    for frame in times:
        bpy.context.scene.frame_set(int(frame))
        world_pos = obj.matrix_world @ bone.head
        keyframes.append({
            "time": (frame - start_frame) / fps,
            "position": [world_pos.x, world_pos.y, world_pos.z]
        })
    
    # 固定ボーン位置（最初のフレーム）
    bpy.context.scene.frame_set(int(start_frame))
    fixed = {}
    for name in fixed_bones:
        if name in obj.pose.bones:
            b = obj.pose.bones[name]
            pos = obj.matrix_world @ b.head
            fixed[name] = [pos.x, pos.y, pos.z]
    
    data = {
        "frameRate": fps,
        "duration": (end_frame - start_frame) / fps,
        "coordinateSystem": "z-up",
        "effector": {
            "bone": effector_bone,
            "keyframes": keyframes
        },
        "fixedBones": fixed
    }
    
    with open(output_path, 'w') as f:
        json.dump(data, f, indent=2)
    
    print(f"Exported to {output_path}")

# 使用例
export_effector_keyframes(
    "Armature",
    "hand.R",
    ["hips", "foot.L", "foot.R"],
    "/tmp/motion.json"
)
```

---

## 変更履歴

| 日付 | 変更内容 |
|------|----------|
| 2025-01-01 | 初版作成 |
