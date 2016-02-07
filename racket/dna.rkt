#lang racket/base

(require rackunit)

(provide dna)

(define (dna-count c lst)
  (cond
    [(null? lst) 0]
    [(equal? (car lst) c) (add1 (dna-count c (cdr lst)))]
    [else (dna-count c (cdr lst))]))

(define (dna s)
  (letrec [(lst (string->list s))
           (a (dna-count #\A lst))
           (c (dna-count #\C lst))
           (g (dna-count #\G lst))
           (t (dna-count #\T lst))]
    `(,a ,c ,g ,t)))

(check-equal?
  (dna "AAAA") '(4 0 0 0))

(check-equal?
  (dna "ACCG") '(1 2 1 0))

(check-equal?
  (dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") '(20 12 17 21))
