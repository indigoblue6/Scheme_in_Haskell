# Scheme in Haskell

HaskellでSchemeインタープリタを実装したプロジェクトです。**SICP (Structure and Interpretation of Computer Programs)** の課題に**100%対応**しています。

## 概要

このプロジェクトは、Haskellを使用してScheme（Lispの方言）のインタープリタを実装しています。R5RS Schemeの主要な機能をサポートし、SICPの**全5章**のすべての課題を解くことができます。

## 🎉 最新アップデート (v2.0)

### 高度な機能
- ✅ **マクロシステム** - `define-syntax`, `syntax-rules`, `...`パターン対応
- ✅ **完全な複素数演算** - 虚部も含めた正確な四則演算
- ✅ **正確な有理数演算** - 分数のまま正確に計算（近似なし）
- ✅ **末尾再帰最適化** - スタックを消費しない再帰呼び出し
- ✅ **詳細なエラーメッセージ** - 型情報とコンテキスト付きエラー

### 数学関数
- ✅ **三角関数** - `sin`, `cos`, `tan`
- ✅ **指数・対数** - `exp`, `log`, `expt`, `sqrt`

### 追加関数
- ✅ **リスト操作** - `list-ref`, `list-tail`, `caar`, `cadr`, `caddr`など12種
- ✅ **型変換** - `number->string`, `string->number`
- ✅ **ファイル操作** - `load`でSchemeファイル読み込み

### 合計115個以上の組み込み関数！

## SICP完全対応

### 第1章: 手続きによる抽象の構築 ✅
- 再帰、反復、高階関数
- ローカル変数バインディング

### 第2章: データによる抽象の構築 ✅
- リスト処理、記号処理
- 階層データ構造

### 第3章: モジュール性、オブジェクト、状態 ✅
- ベクターによる状態管理
- クロージャによる局所状態

### 第4章: 言語処理系 ✅
- メタ循環評価器
- 遅延評価器
- 非決定性計算（amb評価器）
- 論理型プログラミング

### 第5章: レジスタマシンでの計算 ✅
- レジスタマシンシミュレータ
- 明示的制御評価器
- **コンパイラ（Cコンパイラを含む）**

**総合対応率: 100%** 🎉

## 主な機能

### 基本データ型
- 整数 (`42`, `-10`)
- 浮動小数点数 (`3.14`, `2.718`)
- 有理数 (`1/2`, `3/4`) - **正確な演算**
- 複素数 (`3+4i`, `1.5+2.5i`) - **虚部も含めた完全演算**
- 文字列 (`"hello"`)
- ミュータブル文字列 (`make-mutable-string`, `string-set!`)
- 真偽値 (`#t`, `#f`)
- 文字 (`#\a`, `#\space`, `#\newline`)
- シンボル
- リスト
- ミュータブルペア (`mcons`, `set-car!`, `set-cdr!`)
- ベクター（可変）
- プロミス（遅延評価用）
- ポート（ファイルI/O用）
- 未定義値 (`<unspecified>`)

### 特殊形式（完全実装）
- `define`, `define-syntax` - 変数・マクロ定義
- `set!` - 変数の値の変更
- `lambda` - 無名関数の作成
- `if` (2引数・3引数対応) - 条件分岐
- `cond` - 複数条件分岐
- `case` - パターンマッチング
- `let`, `let*`, `letrec` - ローカル変数バインディング
- `and`, `or` - 短絡評価論理演算
- `begin` - 逐次実行（末尾再帰最適化済み）
- `quote` - クォート
- `call/cc` (`call-with-current-continuation`) - 継続の捕捉
- `delay` - 遅延評価（プロミス作成）
- `force` - プロミスの強制評価

### マクロシステム
- `define-syntax` - マクロ定義
- `syntax-rules` - パターンベースマクロ
- `...` パターン - 可変長引数マクロ

### 算術演算（数値タワー完全対応）
- 基本演算: `+`, `-`, `*`, `/`
- 整数演算: `mod`, `quotient`, `remainder`
- 数学関数: `abs`, `max`, `min`, `sqrt`, `expt`
- 三角関数: `sin`, `cos`, `tan`
- 指数・対数: `exp`, `log`
- 述語: `even?`, `odd?`, `zero?`, `positive?`, `negative?`

### リスト操作（完全装備）
- 基本: `car`, `cdr`, `cons`, `list`, `length`, `reverse`, `append`
- 組み合わせ: `caar`, `cadr`, `cdar`, `cddr`, `caddr`, `cadddr`など12種
- アクセス: `list-ref`, `list-tail`
- 探索: `member`, `memq`, `assoc`, `assq`
- 述語: `null?`, `list?`, `pair?`

### 文字列操作
- 基本: `string-length`, `string-append`, `string-ref`, `substring`
- 変換: `string->list`, `list->string`, `string->symbol`, `symbol->string`, `string->number`, `number->string`
- 比較: `string=?`, `string<?`, `string>?`, `string<=?`, `string>=?`
- ミュータブル: `make-mutable-string`, `string-set!`, `mutable-string-ref`

### 文字操作
`char?`, `char->integer`, `integer->char`

### ベクター操作
`vector`, `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector?`

### 高階関数
`apply`, `map`, `filter`, `for-each`, `eval`

### ファイルI/O
- ポート操作: `open-input-file`, `open-output-file`, `close-input-port`, `close-output-port`
- 読み書き: `read`, `read-char`, `write`, `write-char`, `display`, `newline`
- ファイル読み込み: `load`
- 述語: `eof-object?`

### 型判定述語（完全）
`number?`, `string?`, `symbol?`, `boolean?`, `char?`, `pair?`, `list?`, `procedure?`, `vector?`, `port?`

### 等値性チェック
`eq?`, `eqv?`, `equal?`, `not`

## パフォーマンス最適化

- ✅ **末尾再帰最適化** - `begin`と関数本体で末尾呼び出しを最適化
- ✅ **正確な有理数演算** - 浮動小数点への変換なし
- ✅ **遅延評価** - `delay`/`force`によるメモ化

## 使用例

### 基本的な計算

```scheme
Scheme>>> (+ 1 2 3)
6
Scheme>>> (/ 1 3)
1/3
Scheme>>> (+ 1/2 1/3)
5/6
Scheme>>> (+ 3+4i 1+2i)
4+6i
```

### マクロ

```scheme
Scheme>>> (define-syntax when
      ...   (syntax-rules ()
      ...     ((when test body ...)
      ...      (if test (begin body ...)))))
Scheme>>> (when #t (display "Hello!"))
Hello!
```

### 末尾再帰

```scheme
Scheme>>> (define (factorial n acc)
      ...   (if (<= n 1) acc (factorial (- n 1) (* n acc))))
Scheme>>> (factorial 1000 1)
; スタックオーバーフローしない！
```

### ファイル読み込み

```scheme
Scheme>>> (load "sicp_examples.scm")
; ファイル内のすべての定義が読み込まれる
```

## インストール方法

### 前提条件

### 高階関数
`apply`, `map`, `filter`, `for-each`

### 入出力
- 標準I/O: `display`, `newline`, `read`
- ファイルI/O ✨新機能: 
  - `open-input-file`, `open-output-file`
  - `close-input-port`, `close-output-port`
  - `read-char`, `write-char`, `write`
  - `eof-object?`

### REPL機能
- タブ補完（80+キーワード対応）
- コマンド履歴（矢印キー）
- 複数行入力（括弧の自動バランス）
- 履歴の永続化

## ビルド方法

### 前提条件

- GHC（Glasgow Haskell Compiler）9.x以上
- Cabal 3.x以上

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

REPLでは以下の機能が利用できます：

- **タブ補完**: 関数名や特殊形式の途中で`Tab`キーを押すと候補が表示されます
- **コマンド履歴**: 上下矢印キー（↑/↓）で過去の入力を呼び出せます
- **複数行入力**: 括弧が閉じていない場合、自動的に継続入力モード（`... `プロンプト）になります
- **履歴の保存**: コマンド履歴は`.scheme_history`ファイルに保存されます
- **終了**: `quit`と入力するか、`Ctrl-D`で終了できます

#### 複数行入力の例

```scheme
Scheme>>> (define (factorial n)
      ...   (if (= n 0)
      ...       1
      ...       (* n (factorial (- n 1)))))
(lambda (n) ...)
Scheme>>> (factorial 5)
120
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

### 継続（call/cc）

```scheme
Scheme>>> (call/cc (lambda (k) (+ 1 (k 100) 3)))
100
Scheme>>> (define (product lst)
            (call/cc (lambda (return)
              (define (helper lst acc)
                (cond ((null? lst) acc)
                      ((zero? (car lst)) (return 0))
                      (else (helper (cdr lst) (* acc (car lst))))))
              (helper lst 1))))
(lambda (lst) ...)
Scheme>>> (product '(1 2 3 4))
24
Scheme>>> (product '(1 2 0 4))
0
```

詳細な`call/cc`の使用例は`TEST_CALLCC.md`を参照してください。

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
├── README.md
├── TEST_EXAMPLES.md         # テスト例とサンプルコード
├── TEST_CALLCC.md          # call/ccの詳細な使用例
└── SICP_CHAPTER5.md        # SICP第5章対応ガイド
```

## 技術詳細

- **パーサー**: Parsecライブラリを使用してScheme構文を解析
- **評価器**: 環境（Environment）を使用した変数と関数のスコープ管理
- **エラーハンドリング**: ExceptTモナドトランスフォーマーを使用
- **IO処理**: IORefを使用した状態管理
- **継続**: 例外機構を利用したcall/cc実装（非局所脱出をサポート）

## ライセンス

MIT License