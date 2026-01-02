# Humanoid Animation Generator

Unity Humanoid rig 対応のアニメーションクリップを生成するCLIツール。

## 概要

固定ボーン（複数）と動作ボーン（エフェクター、1つ）を指定し、TwoBone IKによって自然な動きを生成します。
Unity Humanoid Muscle形式（.anim）またはglTF形式（.glb）で出力可能です。

## インストール

### NixOS / Nix

```bash
# 開発環境に入る
nix develop

# ビルド
cabal build

# 実行
cabal run humanoid-anim -- --help
```

## コマンド

### generate - アニメーション生成

YAMLまたはBlender JSON設定ファイルからアニメーションを生成します。

```bash
humanoid-anim generate -i <INPUT> -o <OUTPUT> [OPTIONS]
```

**必須オプション：**
- `-i, --input FILE` : 入力設定ファイル（.yaml または .json）
- `-o, --output FILE` : 出力ファイル（.anim または .glb）

**オプション：**
- `-f, --format FORMAT` : 出力形式（`unity` または `gltf`）。省略時は拡張子から自動判定
- `--solver SOLVER` : IKソルバー（`twobone` または `ccd`）。デフォルト: `twobone`
- `--fps N` : フレームレート。デフォルト: 30
- `--frames N` : フレーム数を固定
- `--loop MODE` : ループモード（`once`, `cycle`, `pingpong`）
- `--no-optimize` : キーフレーム最適化を無効化
- `-v, --verbose` : 詳細出力
- `-q, --quiet` : 警告を抑制
- `--strict` : 警告をエラーとして扱う

**例：**
```bash
# Unity .anim 形式で出力
humanoid-anim generate -i squat.yaml -o squat.anim

# glTF 形式で出力（60fps）
humanoid-anim generate -i wave.yaml -o wave.glb --fps 60

# 詳細出力付きで生成
humanoid-anim generate -i walk.yaml -o walk.anim -v
```

### validate - 設定ファイル検証

設定ファイルの構文と内容を検証します。

```bash
humanoid-anim validate -i <INPUT> [--strict]
```

**オプション：**
- `-i, --input FILE` : 検証する設定ファイル
- `--strict` : 警告をエラーとして扱う

**例：**
```bash
humanoid-anim validate -i my_animation.yaml
```

### convert - Blender JSON から YAML への変換

BlenderからエクスポートしたアニメーションデータをこのツールのYAML形式に変換します。

```bash
humanoid-anim convert -i <INPUT> -o <OUTPUT> [-v]
```

**オプション：**
- `-i, --input FILE` : 入力 Blender JSON ファイル
- `-o, --output FILE` : 出力 YAML ファイル
- `-v, --verbose` : 詳細出力

**例：**
```bash
humanoid-anim convert -i blender_export.json -o animation.yaml
```

### info - スケルトン情報表示

サポートしているボーン一覧を表示します。

```bash
humanoid-anim info
```

## 設定ファイル例

### 手を振るアニメーション

```yaml
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
    - time: 1.0
      position: [0.5, 1.5, 0.3]
    - time: 2.0
      position: [0.4, 1.2, 0.2]

config:
  frameRate: 30
  solver: "twobone"
  loop: "cycle"

output:
  format: "unity"
  optimize: true
```

### スクワットアニメーション（Inverse IK）

足を固定し、腰を上下に動かすスクワット動作：

```yaml
name: "Squat"
duration: 2.0

fixed:
  - bone: "LeftFoot"
    position: [-0.1, 0, 0]
  - bone: "RightFoot"
    position: [0.1, 0, 0]

effector:
  bone: "Hips"
  keyframes:
    - time: 0.0
      position: [0, 0.9, 0]
    - time: 0.5
      position: [0, 0.4, -0.3]
    - time: 1.0
      position: [0, 0.9, 0]
    - time: 1.5
      position: [0, 0.4, -0.3]
    - time: 2.0
      position: [0, 0.9, 0]

config:
  frameRate: 30
  solver: "twobone"
  loop: "cycle"
```

## 出力形式

### Unity .anim（Humanoid Muscle）

Unity Humanoid Muscle形式で出力します。Humanoidリグが設定されたアバターに直接適用できます。

- 筋肉値は -1.0 〜 1.0 の範囲で正規化
- Fetus Position を基準（muscle = 0）として相対角度で計算
- RootT/RootQ によるルートモーション対応

### glTF .glb

glTF 2.0 バイナリ形式で出力します。棒人間メッシュとアニメーションを含みます。

## ライセンス

MIT
