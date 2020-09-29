#lang racket/base

(provide poly-add
         poly-sub
         poly-mul)

(define (zeroes n)
  (if (< n 1)
    '()
    (cons 0 (zeroes (- n 1)))))

(define (poly-add as bs)
  "add two polynomials together"
  (cond
    [(null? as) '()]
    [(> (length as) (length bs)) (cons (car as) (poly-add (cdr as) bs))]
    [(< (length as) (length bs)) (cons (car bs) (poly-add as (cdr bs)))]
    [#t (cons (+ (car as) (car bs)) (poly-add (cdr as) (cdr bs)))]))

(define (poly-sub as bs)
  "subtract poly 'bs' from 'as'"
  (poly-add as (map (lambda (x) (- x)) bs)))

(define (poly-mul as bs)
  "multiply two polynomials together"
  (if (null? as)
    as
    (poly-add
      (append (map (lambda (x) (* x (car as))) bs) (zeroes (- (length as) 1)))
      (poly-mul (cdr as) bs))))
