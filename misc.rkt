#lang racket/base

(provide eval-formula
         multi-eval-formula
         fun-sort)

(define (eval-formula sym val formula)
  (eval `(let [(,sym ,val)] ,formula)))

(define (multi-eval-formula bindings formula)
  (eval '(let ,bindings ,formula)))

(define (fun-sort xs . fun)
  "sort the values in xs with fun applied to them"
  (if (null? xs)
    xs
    (let [(fun
            (cond
              [(pair? fun) (car fun)]
              [(null? fun) (lambda (x) x)]
              [#t fun]))]
    (append
      (fun-sort (filter (lambda (x) (<  (fun x) (fun (car xs)))) (cdr xs)) fun)
      (list (car xs))
      (fun-sort (filter (lambda (x) (>=  (fun x) (fun (car xs)))) (cdr xs)) fun)))))
