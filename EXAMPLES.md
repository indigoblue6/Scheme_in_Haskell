# 使用例集

Scheme in Haskellインタープリタの実践的な使用例です。

## 目次

1. [基本的な使い方](#基本的な使い方)
2. [正確な有理数計算](#正確な有理数計算)
3. [複素数の演算](#複素数の演算)
4. [マクロの活用](#マクロの活用)
5. [末尾再帰最適化](#末尾再帰最適化)
6. [継続 (call/cc)](#継続-callcc)
7. [遅延評価](#遅延評価)
8. [ファイルI/O](#ファイルio)

---

## 基本的な使い方

### REPLの起動

```bash
$ cabal run scheme-interpreter
Scheme>>> 
```

### 簡単な計算

```scheme
Scheme>>> (+ 1 2 3)
6

Scheme>>> (* 7 8 9)
504

Scheme>>> (define (square x) (* x x))
Scheme>>> (square 5)
25
```

---

## 正確な有理数計算

浮動小数点への変換なしで、分数のまま正確に計算できます。

```scheme
;; 分数リテラル
Scheme>>> 1/2
1/2

;; 正確な加算
Scheme>>> (+ 1/2 1/3)
5/6

;; 自動的に約分
Scheme>>> (+ 1/4 1/4)
1/2

;; 整数に簡約
Scheme>>> (+ 1/3 1/3 1/3)
1

;; 正確な除算
Scheme>>> (/ 1 3)
1/3

;; 複雑な計算
Scheme>>> (* (+ 1/2 1/3) (- 3/4 1/6))
35/72
```

---

## 複素数の演算

虚部も含めた完全な複素数演算をサポートしています。

```scheme
;; 複素数リテラル
Scheme>>> 3+4i
3+4i

;; 加算
Scheme>>> (+ 1+2i 3+4i)
4+6i

;; 減算
Scheme>>> (- 5+6i 2+3i)
3+3i

;; 乗算（虚数単位の性質を正しく処理）
Scheme>>> (* 2+3i 1+1i)
-1+5i

;; 実数と複素数の混在
Scheme>>> (+ 10 3+4i)
13+4i
```

---

## マクロの活用

### whenマクロ

```scheme
Scheme>>> (define-syntax when
      ...   (syntax-rules ()
      ...     ((when test body ...)
      ...      (if test (begin body ...)))))

Scheme>>> (when #t
      ...   (display "This is true\n")
      ...   (display "Multiple expressions\n"))
This is true
Multiple expressions
```

### unlessマクロ

```scheme
Scheme>>> (define-syntax unless
      ...   (syntax-rules ()
      ...     ((unless test body ...)
      ...      (if (not test) (begin body ...)))))

Scheme>>> (unless #f (display "This will print\n"))
This will print
```

### 可変長引数マクロ（...パターン）

```scheme
Scheme>>> (define-syntax let-values
      ...   (syntax-rules ()
      ...     ((let-values ((var val) ...) body ...)
      ...      (let ((var val) ...) body ...))))

Scheme>>> (let-values ((x 1) (y 2) (z 3))
      ...   (+ x y z))
6
```

---

## 末尾再帰最適化

スタックを消費せずに大きな再帰計算が可能です。

### 階乗（反復版）

```scheme
Scheme>>> (define (factorial n)
      ...   (define (iter n acc)
      ...     (if (<= n 1)
      ...         acc
      ...         (iter (- n 1) (* n acc))))
      ...   (iter n 1))

Scheme>>> (factorial 1000)
; 巨大な数値が返る（スタックオーバーフローしない！）
```

### フィボナッチ（末尾再帰版）

```scheme
Scheme>>> (define (fib n)
      ...   (define (iter a b count)
      ...     (if (= count 0)
      ...         b
      ...         (iter (+ a b) a (- count 1))))
      ...   (iter 1 0 n))

Scheme>>> (fib 100)
354224848179261915075
```

---

## 継続 (call/cc)

継続を使った高度な制御フロー。

### 早期リターン

```scheme
Scheme>>> (define (product lst)
      ...   (call/cc
      ...     (lambda (return)
      ...       (define (iter lst)
      ...         (cond ((null? lst) 1)
      ...               ((= (car lst) 0) (return 0))
      ...               (else (* (car lst) (iter (cdr lst))))))
      ...       (iter lst))))

Scheme>>> (product '(1 2 3 4))
24

Scheme>>> (product '(1 2 0 4))
0  ; 0を見つけたら即座に返る
```

---

## 遅延評価

必要になるまで計算を遅延します。

```scheme
Scheme>>> (define expensive (delay (begin
      ...                             (display "Computing...\n")
      ...                             (+ 1000 2000))))

Scheme>>> (display "Created promise\n")
Created promise

Scheme>>> (force expensive)
Computing...
3000

Scheme>>> (force expensive)
3000  ; 2回目は再計算しない（メモ化）
```

---

## ファイルI/O

### ファイルへの書き込み

```scheme
Scheme>>> (define out (open-output-file "output.txt"))
Scheme>>> (write-char #\H out)
Scheme>>> (write-char #\i out)
Scheme>>> (close-output-port out)
```

### ファイルからの読み込み

```scheme
Scheme>>> (define in (open-input-file "output.txt"))
Scheme>>> (read-char in)
#\H
Scheme>>> (read-char in)
#\i
Scheme>>> (close-input-port in)
```

### ファイルからのコード読み込み

```scheme
; ファイル: my-functions.scm
(define (double x) (* x 2))
(define (triple x) (* x 3))

; REPLで読み込み
Scheme>>> (load "my-functions.scm")
Scheme>>> (double 5)
10
Scheme>>> (triple 5)
15
```

---

## 高度な例: SICPスタイルのプログラミング

### 高階関数

```scheme
Scheme>>> (define (accumulate op initial seq)
      ...   (if (null? seq)
      ...       initial
      ...       (op (car seq)
      ...           (accumulate op initial (cdr seq)))))

Scheme>>> (accumulate + 0 '(1 2 3 4 5))
15

Scheme>>> (accumulate * 1 '(1 2 3 4 5))
120
```

### フィルタリング

```scheme
Scheme>>> (filter even? '(1 2 3 4 5 6 7 8))
(2 4 6 8)

Scheme>>> (filter (lambda (x) (> x 5)) '(1 3 5 7 9))
(7 9)
```

### マッピング

```scheme
Scheme>>> (map (lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)

Scheme>>> (map + '(1 2 3) '(10 20 30))
(11 22 33)
```

---

詳細なAPIリファレンスは`API.md`を、SICPの有名なプログラム例は`sicp_examples.scm`を参照してください。
