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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)  -- 組み込み関数
             | Func { params :: [String]       -- 仮引数
                    , vararg :: Maybe String   -- 可変長引数
                    , body :: [LispVal]        -- 関数本体
                    , closure :: Env           -- クロージャ
                    }
```

#### エラー型

```haskell
data LispError = NumArgs Integer [LispVal]     -- 引数の数が不正
               | TypeMismatch String LispVal    -- 型の不一致
               | Parser ParseError              -- パースエラー
               | BadSpecialForm String LispVal  -- 不正な特殊形式
               | NotFunction String String      -- 関数でない
               | UnboundVar String String       -- 未定義の変数
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
- `parseAtom`: シンボルと真偽値 `#t`, `#f`, `x`, `+`
- `parseNumber`: 数値 `123`
- `parseList`: リスト `(1 2 3)`
- `parseDottedList`: ドット記法 `(1 2 . 3)`
- `parseQuoted`: クォート `'(1 2 3)`

#### パース例

```
入力: "(+ 1 2)"
出力: List [Atom "+", Number 1, Number 2]

入力: "'(a b c)"
出力: List [Atom "quote", List [Atom "a", Atom "b", Atom "c"]]
```

### 3. Eval.hs - 評価器と組み込み関数

#### 評価規則

- **自己評価型**: 文字列、数値、真偽値はそのまま評価される
- **変数**: 環境から値を検索
- **quote**: 引数を評価せずに返す
- **if**: 条件式に基づいて分岐
- **set!**: 既存の変数に新しい値を代入
- **define**: 新しい変数または関数を定義
- **lambda**: 無名関数を作成
- **関数呼び出し**: 関数と引数を評価して適用

#### 組み込み関数

##### 算術演算
- `+`, `-`, `*`, `/`: 基本的な四則演算
- `mod`, `quotient`, `remainder`: 剰余演算

##### 比較演算
- `=`, `<`, `>`, `<=`, `>=`, `/=`: 数値の比較
- `string=?`, `string<?`, etc.: 文字列の比較

##### リスト操作
- `car`: リストの最初の要素を取得
- `cdr`: リストの先頭以外を取得
- `cons`: 要素をリストの先頭に追加

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

このインタープリタは以下の機能で拡張可能です：

1. **浮動小数点数のサポート**
2. **文字型のサポート**
3. **より多くの組み込み関数**（`map`, `filter`, `fold`など）
4. **マクロシステム**
5. **ファイルからのコード読み込み**
6. **ガベージコレクション**（現在はHaskellのGCに依存）
7. **末尾再帰最適化**
8. **コンティニュエーション**

## 制限事項

- 整数のみサポート（浮動小数点数は未サポート）
- 文字型は未サポート
- ファイルI/Oは未実装
- マクロは未実装
- call/ccなどの高度な制御構造は未実装

## 学習リソース

このプロジェクトは以下のリソースを参考にしています：

- "Write Yourself a Scheme in 48 Hours" - Haskellチュートリアル
- R5RS Scheme仕様
- Structure and Interpretation of Computer Programs (SICP)
