#lang racket/base

(provide eval-formula
         multi-eval-formula
         func-sort
         sigma
         DFT)

(define (eval-formula sym val formula)
  "evaluate 'formula' with var 'sym' = 'val'"
  (eval `(let [(,sym ,val)] ,formula)))

(define (multi-eval-formula bindings formula)
  "evaluate 'formula' with let-style 'bindings'"
  (eval '(let ,bindings ,formula)))

(define (func-sort xs fn)
  "quicksort xs with (fn k) being used for comparison"
  (if (< (length xs) 2)
    xs
    (append
      (func-sort (filter (lambda (x) (<  (fn x) (fn (car xs)))) (cdr xs)) fn)
      (list (car xs))
      (func-sort (filter (lambda (x) (>= (fn x) (fn (car xs)))) (cdr xs)) fn))))

(define (sigma a b expr)
  "the sum between a and b (inclusive) of expr(k)"
  (if (> a b)
    0
    (+ `((lambda (k) ,expr) ,a) (sigma (+ a 1) b expr))))

;; Fourier transformation functions

(define (DFT-iterator-sum xs n k)
  (if (null? xs)
    0
    (+
      (* (car xs) (exp (/ (* 0-2i 3.141592653589793 k n) (+ n (length xs)))))
      (DFT-iterator-sum (cdr xs) (+ n 1) k))))

(define (DFT-iterator xs xs-copy k)
    (if (null? xs)
      xs
      (cons
        (DFT-iterator-sum xs-copy 0 k)
        (DFT-iterator (cdr xs) xs-copy (+ k 1)))))

(define (DFT xs)
  "computes the discrete fourier transformation of the list xs"
  (DFT-iterator xs xs 0))
