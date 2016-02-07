#lang racket/base

(require rackunit)

(provide recv)

(define (complement c)
  (cond
    [(eq? c #\A) #\T]
    [(eq? c #\T) #\A]
    [(eq? c #\C) #\G]
    [(eq? c #\G) #\C]
    [else c]))

(define (recv-transcribe lst)
  (if (null? lst)
    '()
    (append (recv-transcribe (cdr lst))
            (list (complement (car lst))))))

(define (recv s)
  (list->string (recv-transcribe (string->list s))))

(check-equal? (recv "GTCA") "TGAC")
(check-equal? (recv "AAAACCCGGT") "ACCGGGTTTT")
