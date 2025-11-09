# Scheme in Haskell

HaskellでSchemeインタープリタを実装したプロジェクトです。

## 概要

このプロジェクトは、Haskellを使用してScheme（Lispの方言）のインタープリタを実装しています。基本的な算術演算、リスト操作、変数定義、関数定義、条件分岐などの機能をサポートしています。

## 機能

- **基本データ型**: 数値、文字列、真偽値、シンボル、リスト
- **算術演算**: `+`, `-`, `*`, `/`, `mod`, `quotient`, `remainder`
- **比較演算**: `=`, `<`, `>`, `<=`, `>=`, `/=`
- **リスト操作**: `car`, `cdr`, `cons`
- **等値性チェック**: `eq?`, `eqv?`, `equal?`
- **変数定義**: `define`
- **変数への代入**: `set!`
- **条件分岐**: `if`
- **関数定義**: `lambda`
- **クォート**: `'` または `quote`
- **REPL**: 対話型環境

## ビルド方法

### 前提条件

- GHC（Glasgow Haskell Compiler）
- Cabal

### Ubuntu/Debianへのインストール

Haskell開発環境をUbuntuにインストールする手順：

```bash
# 必要なビルドツールと依存関係をインストール
sudo apt update
sudo apt install -y build-essential curl libffi-dev libgmp-dev libncurses-dev libtinfo-dev zlib1g-dev

# GHCupをインストール（GHC、Cabal、Stackの管理ツール）
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash

# シェルを再起動するか、以下のコマンドでパスを反映
source ~/.bashrc  # bashの場合
# または
source ~/.zshrc   # zshの場合
```

インストール後、以下のコマンドでバージョンを確認できます：

```bash
ghc --version
cabal --version
```

### ビルド

```bash
cabal build
```

## 実行方法

### REPL（対話型環境）

```bash
cabal run scheme-interpreter
```

### 単一の式を評価

```bash
cabal run scheme-interpreter "(+ 2 3)"
```

## 使用例

### 算術演算

```scheme
Scheme>>> (+ 2 3)
5
Scheme>>> (* 4 5)
20
Scheme>>> (- 10 3)
7
```

### 変数定義

```scheme
Scheme>>> (define x 10)
10
Scheme>>> (define y 20)
20
Scheme>>> (+ x y)
30
```

### 関数定義

```scheme
Scheme>>> (define (square x) (* x x))
(lambda (x) ...)
Scheme>>> (square 5)
25
Scheme>>> (define (add a b) (+ a b))
(lambda (a b) ...)
Scheme>>> (add 3 7)
10
```

### リスト操作

```scheme
Scheme>>> (cons 1 (cons 2 (cons 3 '())))
(1 2 3)
Scheme>>> (car '(1 2 3))
1
Scheme>>> (cdr '(1 2 3))
(2 3)
```

### 条件分岐

```scheme
Scheme>>> (if (> 5 3) "yes" "no")
"yes"
Scheme>>> (if (< 5 3) "yes" "no")
"no"
```

### ラムダ式

```scheme
Scheme>>> ((lambda (x) (* x 2)) 5)
10
Scheme>>> (define double (lambda (x) (* x 2)))
(lambda (x) ...)
Scheme>>> (double 7)
14
```

## プロジェクト構造

```
.
├── scheme-interpreter.cabal  # Cabalプロジェクト定義
├── src/
│   ├── Main.hs              # メインプログラムとREPL
│   ├── LispVal.hs           # データ型と環境管理
│   ├── Parser.hs            # Schemeコードのパーサー
│   └── Eval.hs              # 評価器と組み込み関数
├── LICENSE
└── README.md
```

## 技術詳細

- **パーサー**: Parsecライブラリを使用してScheme構文を解析
- **評価器**: 環境（Environment）を使用した変数と関数のスコープ管理
- **エラーハンドリング**: ExceptTモナドトランスフォーマーを使用
- **IO処理**: IORefを使用した状態管理

## ライセンス

MIT License