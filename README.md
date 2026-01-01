# Humanoid Animation Generator

Unity Humanoid rig 対応のアニメーションクリップを生成するCLIツール。

## 概要

固定ボーン（複数）と動作ボーン（エフェクター、1つ）を指定し、IK（FABRIK）によって自然な動きを生成します。

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

## 使い方

### アニメーション生成

```bash
humanoid-anim generate -i wave_hand.yaml -o wave_hand.glb
```

### 設定ファイル検証

```bash
humanoid-anim validate -i wave_hand.yaml
```

## 設定ファイル例

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
```

## ライセンス

MIT
