# API Reference

Scheme in Haskellインタープリタの完全なAPI リファレンスです。

## 目次

1. [数値演算](#数値演算)
2. [リスト操作](#リスト操作)
3. [文字列操作](#文字列操作)
4. [高階関数](#高階関数)
5. [制御構造](#制御構造)
6. [マクロ](#マクロ)
7. [I/O操作](#io操作)
8. [型判定](#型判定)

---

## 数値演算

### 基本演算

#### `(+ num ...)`
数値の加算。複素数・有理数対応。
```scheme
(+ 1 2 3)        ; => 6
(+ 1/2 1/3)      ; => 5/6
(+ 1+2i 3+4i)    ; => 4+6i
```

#### `(- num ...)`
数値の減算。
```scheme
(- 5 3)          ; => 2
(- 10 3 2)       ; => 5
```

#### `(* num ...)`
数値の乗算。
```scheme
(* 2 3 4)        ; => 24
(* 2/3 3/4)      ; => 1/2
```

#### `(/ num ...)`
数値の除算。有理数は正確に計算。
```scheme
(/ 10 3)         ; => 10/3
(/ 1 2)          ; => 1/2
(/ 6 2)          ; => 3
```

### 整数演算

#### `(quotient n m)`
整数の商。
```scheme
(quotient 10 3)  ; => 3
```

#### `(remainder n m)`
整数の剰余。
```scheme
(remainder 10 3) ; => 1
```

#### `(mod n m)`
整数の剰余（負数にも対応）。
```scheme
(mod 10 3)       ; => 1
```

### 数学関数

#### `(sqrt num)`
平方根を計算。
```scheme
(sqrt 16)        ; => 4.0
(sqrt 2)         ; => 1.4142135623730951
```

#### `(expt base exp)`
べき乗を計算。
```scheme
(expt 2 10)      ; => 1024
(expt 2.0 0.5)   ; => 1.4142135623730951
```

#### `(sin num)`, `(cos num)`, `(tan num)`
三角関数。
```scheme
(sin 0)          ; => 0.0
(cos 0)          ; => 1.0
(tan (/ 3.14159 4)) ; => ~1.0
```

#### `(exp num)`, `(log num)`
指数関数と自然対数。
```scheme
(exp 1)          ; => 2.718281828459045
(log 2.718281828459045) ; => 1.0
```

### 数値述語

#### `(even? num)`, `(odd? num)`
偶数・奇数判定。
```scheme
(even? 4)        ; => #t
(odd? 3)         ; => #t
```

#### `(zero? num)`, `(positive? num)`, `(negative? num)`
数値の符号判定。
```scheme
(zero? 0)        ; => #t
(positive? 5)    ; => #t
(negative? -3)   ; => #t
```

---

## リスト操作

### 基本操作

#### `(car list)`
リストの最初の要素を取得。
```scheme
(car '(1 2 3))   ; => 1
```

#### `(cdr list)`
リストの先頭以外を取得。
```scheme
(cdr '(1 2 3))   ; => (2 3)
```

#### `(cons elem list)`
要素をリストの先頭に追加。
```scheme
(cons 1 '(2 3))  ; => (1 2 3)
```

### car/cdr組み合わせ

#### `(caar list)`, `(cadr list)`, `(cdar list)`, `(cddr list)`
2段階のcar/cdr組み合わせ。
```scheme
(cadr '(1 2 3))  ; => 2  (car (cdr ...))
(cdar '((1 2) 3)) ; => (2) (cdr (car ...))
```

#### `(caddr list)`, `(cadddr list)` など
3段階以上の組み合わせも利用可能。
```scheme
(caddr '(1 2 3 4)) ; => 3  (car (cdr (cdr ...)))
```

### リスト構築・操作

#### `(list elem ...)`
リストを作成。
```scheme
(list 1 2 3)     ; => (1 2 3)
```

#### `(length list)`
リストの長さを取得。
```scheme
(length '(1 2 3 4)) ; => 4
```

#### `(reverse list)`
リストを反転。
```scheme
(reverse '(1 2 3)) ; => (3 2 1)
```

#### `(append list ...)`
リストを連結。
```scheme
(append '(1 2) '(3 4)) ; => (1 2 3 4)
```

#### `(list-ref list k)`
k番目の要素を取得（0始まり）。
```scheme
(list-ref '(a b c d) 2) ; => c
```

#### `(list-tail list k)`
k番目以降のリストを取得。
```scheme
(list-tail '(a b c d) 2) ; => (c d)
```

### リスト検索

#### `(member obj list)`
リスト内の要素を検索。
```scheme
(member 'b '(a b c)) ; => (b c)
(member 'x '(a b c)) ; => #f
```

#### `(assoc key alist)`
連想リストから要素を検索。
```scheme
(assoc 'b '((a 1) (b 2) (c 3))) ; => (b 2)
```

### リスト述語

#### `(null? obj)`
空リスト判定。
```scheme
(null? '())      ; => #t
(null? '(1))     ; => #f
```

#### `(list? obj)`
リスト判定。
```scheme
(list? '(1 2 3)) ; => #t
(list? 'symbol)  ; => #f
```

#### `(pair? obj)`
ペア判定。
```scheme
(pair? '(1 . 2)) ; => #t
(pair? '(1 2))   ; => #t
```

---

## 制御構造

### 条件分岐

#### `(if test conseq [alt])`
条件分岐。altを省略した場合、偽のとき`<unspecified>`を返す。
```scheme
(if #t 'yes 'no)  ; => yes
(if #f 'yes)      ; => <unspecified>
```

#### `(cond clause ...)`
複数条件分岐。
```scheme
(cond ((< x 0) 'negative)
      ((> x 0) 'positive)
      (else 'zero))
```

#### `(case key clause ...)`
値による分岐。
```scheme
(case x
  ((1 2) 'small)
  ((3 4) 'medium)
  (else 'large))
```

### ループ・再帰

#### `(begin expr ...)`
複数の式を順次実行。末尾再帰最適化済み。
```scheme
(begin
  (display "Hello")
  (newline)
  42)  ; => 42
```

---

## マクロ

#### `(define-syntax name (syntax-rules () pattern ...))`
マクロを定義。
```scheme
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))
```

#### `...` パターン
可変長引数のマクロパターン。
```scheme
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((var val) ...) body ...)
     (let ((var val) ...) body ...))))
```

---

## I/O操作

### 表示

#### `(display obj)`
オブジェクトを表示（エスケープシーケンス処理あり）。
```scheme
(display "Hello\n") ; => Hello (改行)
```

#### `(newline)`
改行を出力。
```scheme
(newline) ; => 
```

### ファイルI/O

#### `(load filename)`
Schemeファイルを読み込んで実行。
```scheme
(load "examples.scm")
```

#### `(open-input-file filename)`
入力用ファイルを開く。
```scheme
(define port (open-input-file "test.txt"))
```

#### `(read-char [port])`
1文字読み込み。
```scheme
(read-char port) ; => #\a
```

---

## 型判定

### 基本型

#### `(number? obj)`
数値判定（整数・有理数・複素数・浮動小数点）。

#### `(string? obj)`
文字列判定。

#### `(symbol? obj)`
シンボル判定。

#### `(boolean? obj)`
真偽値判定。

#### `(char? obj)`
文字判定。

#### `(procedure? obj)`
手続き判定。

---

完全なリストは115個以上の関数を含みます。詳細は`src/Eval.hs`の`primitives`と`ioPrimitives`を参照してください。
