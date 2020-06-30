#lang racket/base

(require racket/list
         "misc.rkt")

(provide differentiate
         tidied-differentiate
         diff-func)

;;; UTILS ;;;

(define (expr-contains? expr var)
 "Recursively iterates through an expression (of any number of dimensions)
  to see if it contains var"
  (cond
    [(null? expr) #f]
    [(pair? expr)
      (let ([above (expr-contains? (car expr) var)])
        (if above
          above
          (expr-contains? (cdr expr) var)))]
    [(equal? expr var) #t]
    [#t #f]))

(define (contains-symbol? xs)
  (cond
    [(null? xs) #f]
    [(symbol? (car xs)) #t]
    [#t (contains-symbol? (cdr xs))]))

(define (remove-all n xs)
  (cond
    [(null? xs) xs]
    [(equal? (car xs) n) (remove-all n (cdr xs))]
    [#t (cons (car xs) (remove-all n (cdr xs)))]))

(define (contains? n xs)
  (cond
    [(null? xs) #f]
    [(equal? (car xs) n) #t]
    [#t (contains? n (cdr xs))]))

;;; TIDYING ;;;

(define (tidy expr)
  (define (simplify sym vals)
    (let* ([numbers (filter (lambda (x) (number? x)) vals)]
           [num (eval (cons sym numbers))]
           [first-two (list sym num)])
    (append first-two (filter (lambda (x) (not (number? x))) vals))))
  ; main body
  (let* ([symbol (car expr)]
         [values (cdr expr)]
         [new-expr 
    (cond
      [(equal? symbol '*)
        (let ([simplified (simplify symbol values)])
          (if (contains? 0 simplified)
            0
            (remove-all 1 simplified)))]
      [(equal? symbol '+)
        (let ([simplified (simplify symbol values)])
          (if (= (length simplified) 2)
            simplified
            (filter (lambda (x) (not (equal? x 0))) simplified)))]
      [#t expr])])

  (if (and (pair? new-expr) (= (length new-expr) 2))
    (second new-expr)
    new-expr)))

(define (tidy-recur expr)
  (cond
    [(pair? expr) (tidy (map tidy-recur expr))]
    [#t expr]))

;;; DIFFERENTIATION ;;;

(define (expt-diff base exponent var)
  "returns d/d{var} of base^exponent, with procedures for f(x)^n and n^f(x)"
  (if (expr-contains? base var)
    `(* ,exponent (expt ,base (- ,exponent 1)) ,(differentiate base var))
    `(* ,(differentiate exponent var) (expt ,base ,var) (log ,base))))

(define (diff-product uexpr var)
  "returns d/d{var} of the product of an arbitrarily long list of expressions"
  (define (diff-product-recur xs var)
    "the recursive helper-function of diff-product"
    (if (= (length xs) 1)
      (list (list (differentiate (car xs) var)))
      (let ((diff-rest (diff-product-recur (cdr xs) var)))
        (cons
          (cons
            (differentiate (car xs) var)
            (cdr xs))
          (map (lambda (x) (cons (car xs) x)) diff-rest)))))
  (cons
    `+
    (map (lambda (x) (cons `* x)) (diff-product-recur uexpr var))))

(define (diff-fraction u v var)
  "Simpy applies the quotient rule to get d/d{var} of u/v"
  `(/
    (- (* ,(differentiate u var) ,v) (* ,u ,(differentiate v var)))
    (expt ,v 2)))

;;; EXPORTED ;;;

(define (differentiate expr var)
  "Symbolically differentiates a given s-expression"
  (if (pair? expr)
    ; #t
    (let ([sym  (car expr)]
          [vals (cdr expr)])
    (cond
      [(equal? sym 'expt) (expt-diff (first vals) (second vals) var)]
      [(equal? sym '+)    (cons sym (map (lambda (x) (differentiate x var)) vals))]
      [(equal? sym '*)    (diff-product vals var)]
      [(equal? sym '/)    (diff-fraction (first vals) (second vals) var)]
      [(equal? sym 'sin) `(* (cos ,(car vals)) ,(differentiate (car vals) var))]
      [(equal? sym 'cos) `(- (* (sin ,(car vals)) ,(differentiate (car vals) var)))]
      [(equal? sym 'log) `(/ ,(differentiate (car vals) var) ,(car vals))]
      [#t                 (differentiate sym var)]))
    ; #f
    (if (symbol? expr)
      1
      0)))

(define (tidied-differentiate expr var)
  (tidy-recur  (differentiate expr var)))

(define (diff-func expr var)
  (lambda (x) (eval-formula var x (tidied-differentiate expr var))))
