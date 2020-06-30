#lang racket/base

(provide get-indices)

(define (opposite-sym sym)
  (cond
    ((equal? '+ sym) '-)
    ((equal? '- sym) '+)
    ((equal? '* sym) '/)
    ((equal? '/ sym) '*)
    ((equal? '= sym) '=)))

(define (exclude-index xs i current-i)
  (if (< current-i i)
    (cons (car xs) (exclude-index (cdr xs) i (+ current-i 1)))
    (cdr xs)))

(define (get-indices expr i sym)
  (cond
    [(null? expr)
      #f]
    [(pair? (car expr))
      (let [(evaluated-child (get-indices (car expr) 0 sym))]
        (if evaluated-child
          (cons i evaluated-child)
          (get-indices (cdr expr) (+ i 1) sym)))]
    [(equal? (car expr) sym)
      (list i)]
    [#t
      (get-indices (cdr expr) (+ i 1) sym)]))
