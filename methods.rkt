#lang racket/base

(require "calculus.rkt"
         "misc.rkt")

(provide newton-raphson)

(define (newton-raphson expr start)
  (define (nr-iter fun deriv prev)
    (let* ([quot (/ (fun prev) (deriv prev))]
           [next (- prev quot)])
      (if (= next prev)
;        (- next)
        next
        (nr-iter fun deriv next))))
  (let* ([fun        (lambda (x) (eval-formula expr x))]
         [deriv-expr (differentiate expr 'x)]
         [deriv      (lambda (x) (eval-formula deriv-expr x))])
    (nr-iter fun deriv start)))
