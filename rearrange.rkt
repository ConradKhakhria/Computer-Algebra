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
  "exclude the element at index 'i' from the list xs"
  (if (< current-i i)
    (cons (car xs) (exclude-index (cdr xs) i (+ current-i 1)))
    (cdr xs)))

(define (get-indices expr i sym)
  "gets the paths through 'expr' to sym"
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
      
(define (rearrange equation var)
  "rearrange 'equation' for variable 'var'"
  (define (rearr-iter expr path other-side)
    (if (pair? expr)
      (rearr-iter
        (list-ref expr (car path))
        (cdr path)
        (cons
          `,(opposite-sym (car expr))
          (cons
            `,other-side
            `,(exclude-index (cdr expr) (car path) 0))))
      other-side))
  (let ([path (get-indices equation 0 var)])
    (rearr-iter
      (list-ref equation (car path))
      (cdr path)
      (exclude-index (cdr equation) (car path) 0))))
     
