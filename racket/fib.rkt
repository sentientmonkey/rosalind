#lang racket/base

(require rackunit)

(provide fib)

(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       (let ([results (make-hash)])
         (lambda (args ...)
           (hash-ref! results (list args ...)
                      (lambda () bodies ...)))))]))

(define-memoized (fib n k)
  (if (< n 3)
    1
    (+ (fib (- n 1) k)
       (* k (fib (- n 2) k)))))

(check-equal? (fib 0 1) 1)
(check-equal? (fib 1 1) 1)
(check-equal? (fib 12 1) 144)
(check-equal? (fib 5 3) 19)
