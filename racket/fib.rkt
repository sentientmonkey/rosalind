#lang racket/base

(require rackunit)

(provide fib mfib dfib)

(define (fib n k)
  (if (< n 3)
    1
    (+ (fib (- n 1) k)
       (* k (fib (- n 2) k)))))

(check-equal? (fib 1 1) 1)
(check-equal? (fib 2 1) 1)
(check-equal? (fib 12 1) 144)
(check-equal? (fib 5 3) 19)

(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       (let ([results (make-hash)])
         (lambda (args ...)
           (hash-ref! results (list args ...)
                      (lambda () bodies ...)))))]))

(define-memoized (mfib n k)
  (if (< n 3)
    1
    (+ (mfib (- n 1) k)
       (* k (mfib (- n 2) k)))))

(check-equal? (mfib 1 1) 1)
(check-equal? (mfib 2 1) 1)
(check-equal? (mfib 12 1) 144)
(check-equal? (mfib 5 3) 19)

(define (dfib n k)
  (let ([v (make-vector n 1)])
    (for ([i (in-range 2 n)])
      (vector-set! v i (+ (vector-ref v (- i 1))
                          (* k (vector-ref v (- i 2))))))
    (vector-ref v (sub1 n))))

(check-equal? (dfib 1 1) 1)
(check-equal? (dfib 2 1) 1)
(check-equal? (dfib 12 1) 144)
(check-equal? (dfib 5 3) 19)
