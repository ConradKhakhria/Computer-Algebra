#lang racket/base

(provide poly-add
         poly-sub
         poly-mul)

(define (poly-add a b)
  (cond
    ((or (null? a) (null? b))
      '())
    ((< (length a) (length b))
      (cons (car b) (poly-add a (cdr b))))
    ((> (length a) (length b))
      (cons (car a) (poly-add (cdr a) b)))
    (#t
      (cons (+ (car a) (car b)) (poly-add (cdr a) (cdr b))))))

(define (poly-sub x y)
  (define (negative-list xs)
    (if (null? xs)
      '()
      (cons (- (car xs)) (negative-list (cdr xs)))))
  (poly-add x (negative-list y))) 

(define (poly-mul x y)
  (define (zeroes n)
    (if (= 0 n)
      '()
      (cons 0 (zeroes (- n 1)))))
  (if (null? y)
    (list 0)
    (poly-add 
      (append (map (lambda (z) (* z (car y))) x) (zeroes (- (length y) 1)))
      (poly-mul x (cdr y)))))
