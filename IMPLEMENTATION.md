# Scheme in Haskell - 実装詳細

## アーキテクチャ

このSchemeインタープリタは以下の4つの主要なモジュールで構成されています：

### 1. LispVal.hs - データ型と環境管理

#### データ型定義

```haskell
data LispVal = Atom String              -- シンボル
             | List [LispVal]           -- リスト
             | DottedList [LispVal] LispVal  -- ドット記法リスト
             | Number Integer           -- 整数
             | String String            -- 文字列
             | Bool Bool                -- 真偽値
             | Character Char           -- 文字
             | Float Double             -- 浮動小数点数
             | Rational Integer Integer -- 正確な有理数 (分子, 分母)
             | Complex LispVal LispVal  -- 複素数 (実部, 虚部)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)  -- 組み込み関数
             | IOFunc ([LispVal] -> IOThrowsError LispVal)       -- IO関数
             | Port Handle              -- ファイルハンドル
             | Func { params :: [String]       -- 仮引数
                    , vararg :: Maybe String   -- 可変長引数
                    , body :: [LispVal]        -- 関数本体
                    , closure :: Env           -- クロージャ
                    }
             | Macro { params :: [String]      -- マクロ仮引数
                     , vararg :: Maybe String  -- 可変長引数
                     , body :: [LispVal]       -- マクロ本体
                     , closure :: Env          -- クロージャ
                     }
             | MutablePair (IORef LispVal) (IORef LispVal)  -- 可変ペア
             | MutableString (IORef String)                  -- 可変文字列
             | Promise (IORef (Maybe LispVal))               -- 遅延評価用
             | Continuation Env ([LispVal] -> IOThrowsError LispVal)  -- 継続
             | Unspecified              -- 未規定の値（defineなどの戻り値）
```

#### エラー型

```haskell
data LispError = NumArgs Integer [LispVal]     -- 引数の数が不正
               | TypeMismatch String LispVal    -- 型の不一致
               | Parser ParseError              -- パースエラー
               | BadSpecialForm String LispVal  -- 不正な特殊形式
               | NotFunction String String      -- 関数でない
               | UnboundVar String String       -- 未定義の変数
               | DivisionByZero                 -- ゼロ除算
               | StackOverflow                  -- スタックオーバーフロー
               | RuntimeError String            -- 実行時エラー
               | Default String                 -- その他のエラー
```

#### 環境管理

環境は変数名とその値のマッピングを管理します。`IORef`を使用して状態を持つことができます。

- `nullEnv`: 空の環境を作成
- `isBound`: 変数が定義されているかチェック
- `getVar`: 変数の値を取得
- `setVar`: 既存の変数に値を設定
- `defineVar`: 新しい変数を定義
- `bindVars`: 複数の変数を一度にバインド

### 2. Parser.hs - Schemeコードの構文解析

Parsecライブラリを使用して、テキストをLispValデータ構造に変換します。

#### 主要なパーサー

- `parseString`: 文字列リテラル `"hello"`
- `parseCharacter`: 文字リテラル `#\a`, `#\space`, `#\newline`
- `parseAtom`: シンボルと真偽値 `#t`, `#f`, `x`, `+`
- `parseNumber`: 整数 `123`, 有理数 `1/2`, 複素数 `3+4i`, 浮動小数点数 `3.14`
- `parseList`: リスト `(1 2 3)`
- `parseDottedList`: ドット記法 `(1 2 . 3)`
- `parseQuoted`: クォート `'(1 2 3)`
- `parseQuasiQuoted`: 準クォート `` `(1 ,x) ``
- `parseUnQuote`: アンクォート `,x`

#### パース例

```
入力: "(+ 1 2)"
出力: List [Atom "+", Number 1, Number 2]

入力: "'(a b c)"
出力: List [Atom "quote", List [Atom "a", Atom "b", Atom "c"]]

入力: "1/2"
出力: Rational 1 2

入力: "3+4i"
出力: Complex (Number 3) (Number 4)
```

### 3. Eval.hs - 評価器と組み込み関数

#### 評価規則

- **自己評価型**: 文字列、数値、真偽値、文字はそのまま評価される
- **変数**: 環境から値を検索
- **quote**: 引数を評価せずに返す
- **quasiquote**: 準クォート（一部の要素のみ評価）
- **if**: 条件式に基づいて分岐
- **cond**: 複数条件の分岐
- **case**: パターンマッチング
- **set!**: 既存の変数に新しい値を代入
- **define**: 新しい変数または関数を定義
- **define-syntax**: マクロを定義
- **lambda**: 無名関数を作成
- **begin**: 複数の式を順次評価（末尾再帰最適化あり）
- **let**, **let***, **letrec**: ローカル変数バインディング
- **and**, **or**: 短絡評価論理演算
- **call/cc**: 継続を捕捉
- **delay**, **force**: 遅延評価
- **関数呼び出し**: 関数と引数を評価して適用（末尾呼び出し最適化あり）

#### マクロシステム

`define-syntax`と`syntax-rules`によるハイジェニックマクロをサポート:

- パターンマッチング: `(when test body ...)`
- 可変長パターン: `...` (ellipsis) により任意個の引数をマッチ
- テンプレート展開: マクロ展開時にコードを生成
- マクロ展開: 評価前にマクロを完全に展開

例:
```scheme
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))
```

#### 組み込み関数

##### 算術演算
- `+`, `-`, `*`, `/`: 基本的な四則演算（有理数・複素数対応）
- `mod`, `quotient`, `remainder`: 剰余演算
- `sqrt`: 平方根
- `expt`: 累乗
- `sin`, `cos`, `tan`: 三角関数
- `log`, `exp`: 対数・指数関数
- `abs`, `floor`, `ceiling`, `truncate`, `round`: 数値操作

##### 正確な有理数演算
- 分数リテラル: `1/2`, `3/4`
- 自動約分: `(+ 1/4 1/4)` → `1/2`
- 正確な計算: 浮動小数点への変換なし
- 整数への簡約: `(+ 1/3 1/3 1/3)` → `1`

##### 複素数演算
- 複素数リテラル: `3+4i`, `1-2i`
- 虚部を含む演算: `(* 2+3i 1+1i)` → `-1+5i`
- 実数との混在: `(+ 10 3+4i)` → `13+4i`

##### 比較演算
- `=`, `<`, `>`, `<=`, `>=`, `/=`: 数値の比較
- `string=?`, `string<?`, etc.: 文字列の比較

##### リスト操作
- `car`, `cdr`: 基本的なリスト分解
- `cons`: 要素をリストの先頭に追加
- `list`: 複数の要素からリストを作成
- `length`: リストの長さ
- `append`: リストの結合
- `reverse`: リストの反転
- `list-ref`: インデックスで要素を取得
- `list-tail`: n番目以降の要素
- `caar`, `cadr`, `caddr`, `cadddr`, etc.: car/cdrの組み合わせ (12種類)

##### 高階関数
- `map`: リストの各要素に関数を適用
- `filter`: 条件に合う要素を抽出
- `fold`: リストを畳み込み

##### 型述語
- `number?`, `string?`, `boolean?`, `char?`, `list?`, `pair?`, `symbol?`, `procedure?`
- `null?`, `eof-object?`

##### 型変換
- `number->string`: 数値を文字列に変換
- `string->number`: 文字列を数値に変換
- `symbol->string`, `string->symbol`: シンボルと文字列の相互変換
- `char->integer`, `integer->char`: 文字とASCIIコードの変換

##### I/O関数
- `open-input-file`, `open-output-file`: ファイルを開く
- `close-input-port`, `close-output-port`: ファイルを閉じる
- `read`, `write`, `display`: 読み書き
- `read-char`, `write-char`: 文字の読み書き
- `load`: ファイルからコードを読み込んで実行

##### 等値性
- `eq?`, `eqv?`: 参照の等価性
- `equal?`: 構造的等価性

### 4. Main.hs - REPL と実行環境

#### REPL (Read-Eval-Print Loop)

```
1. Read: ユーザーからの入力を読み取る（Haskellineを使用）
2. Parse: 入力をLispValに変換
3. Eval: LispValを評価
4. Print: 結果を表示
5. Loop: 1に戻る
```

#### REPL機能

- **タブ補完**: Schemeの関数名や特殊形式を補完
- **コマンド履歴**: 矢印キーで過去の入力を呼び出し
- **履歴の永続化**: `.scheme_history`ファイルに保存

#### 実行モード

1. **対話モード**: 引数なしで起動するとREPLが開始
2. **ワンショットモード**: 引数として式を渡すと評価して終了

## 使用技術

### Parsec
テキストのパーサーを簡潔に記述できるパーサーコンビネータライブラリ。

### Haskeline
Readline風のインターフェースを提供するライブラリ。タブ補完、コマンド履歴、行編集機能を実装。

### ExceptT モナドトランスフォーマー
エラーハンドリングとIO処理を組み合わせるために使用。

### IORef
可変状態を管理するために使用。環境（変数のマッピング）の実装に必須。

### Data.Map
効率的な連想配列として環境の実装に使用。

## 拡張可能な機能

このインタープリタは以下の高度な機能を実装済みです：

1. ✅ **正確な有理数のサポート** - 分数を浮動小数点に変換せずに正確に計算
2. ✅ **完全な複素数のサポート** - 虚部を含む四則演算
3. ✅ **浮動小数点数のサポート** - Double型の浮動小数点数
4. ✅ **文字型のサポート** - `#\a`, `#\space`, `#\newline`など
5. ✅ **多数の組み込み関数** - `map`, `filter`, `fold`など115個以上
6. ✅ **マクロシステム** - `define-syntax`と`syntax-rules`、可変長パターン(`...`)対応
7. ✅ **ファイルからのコード読み込み** - `load`関数
8. ✅ **ガベージコレクション** - HaskellのGCに依存
9. ✅ **末尾再帰最適化** - `begin`と関数本体で最適化
10. ✅ **継続** - `call/cc`によるfirst-class continuations
11. ✅ **遅延評価** - `delay`と`force`
12. ✅ **ファイルI/O** - ポート、read/write操作
13. ✅ **準クォート** - quasiquote、unquote、unquote-splicing
14. ✅ **可変データ構造** - `set-car!`, `set-cdr!`
15. ✅ **強化されたエラーメッセージ** - 型情報とコンテキストを含む

## 実装の特徴

### 1. スマート算術システム

異なる数値型を自動的に適切な型に変換:

- 整数 + 整数 → 整数
- 整数 + 有理数 → 有理数
- 有理数 + 有理数 → 有理数（自動約分）
- 実数 + 複素数 → 複素数
- 任意の型 + 浮動小数点 → 浮動小数点

### 2. 末尾呼び出し最適化

末尾位置の関数呼び出しはスタックを消費せず、反復に変換されます:

```haskell
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
evalBody env [expr] = eval env expr  -- 末尾位置: 最適化される
evalBody env (x:xs) = do
  eval env x  -- 非末尾位置: 通常の評価
  evalBody env xs
```

### 3. マクロ展開システム

マクロは評価前に完全に展開されます:

1. パターンマッチング: マクロ呼び出しをパターンにマッチ
2. `...`処理: 可変長引数を正しく展開
3. テンプレート生成: マッチした値をテンプレートに埋め込み
4. 再帰的展開: 結果に含まれるマクロも展開

### 4. 継続の実装

`call/cc`は現在の継続をfirst-classの値として捕捉:

```haskell
Continuation :: Env -> ([LispVal] -> IOThrowsError LispVal) -> LispVal
```

継続を呼び出すと、捕捉した地点に制御が戻ります。

## 制限事項

現在のバージョンでの制限:

- 数値の範囲はHaskellの`Integer`と`Double`に依存
- ガベージコレクションはHaskellのランタイムに依存（手動GCなし）
- 並行処理機能は未実装（threads, futuresなど）
- 一部のR5RS標準ライブラリ関数は未実装
- JITコンパイルやバイトコードコンパイルは未実装（インタープリタのみ）

## パフォーマンス最適化

### 末尾再帰最適化

末尾位置の再帰呼び出しはスタックを消費せず、大きな再帰でもスタックオーバーフローしません:

```scheme
(define (factorial n)
  (define (iter n acc)
    (if (<= n 1)
        acc
        (iter (- n 1) (* n acc))))  ; 末尾呼び出し
  (iter n 1))

(factorial 10000)  ; スタックオーバーフローしない
```

### 遅延評価とメモ化

`delay`で作成されたPromiseは最初の`force`で評価され、結果がキャッシュされます:

```scheme
(define p (delay (expensive-computation)))
(force p)  ; 計算実行
(force p)  ; キャッシュから返す（再計算しない）
```

## 学習リソース

このプロジェクトは以下のリソースを参考にしています：

- "Write Yourself a Scheme in 48 Hours" - Haskellチュートリアル
- R5RS Scheme仕様
- Structure and Interpretation of Computer Programs (SICP)
