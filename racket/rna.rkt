#lang racket/base

(require rackunit)

(provide rna)

(define (rna-transcribe lst)
  (cond
    [(null? lst) '()]
    [(equal? (car lst) #\T) (cons #\U (rna-transcribe (cdr lst)))]
    [else (cons (car lst) (rna-transcribe (cdr lst)))]))

(define (rna str)
  (list->string (rna-transcribe (string->list str))))

(check-equal?
  (rna "ACG") "ACG")

(check-equal?
  (rna "ACGT") "ACGU")

(check-equal?
  (rna "GATGGAACTTGACTACGTAAATT") "GAUGGAACUUGACUACGUAAAUU")
