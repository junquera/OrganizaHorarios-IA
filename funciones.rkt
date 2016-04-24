#lang racket
(define (put n l x)
  (if (and (list? l) (and (>= n 0) (<= n (length l))))
      (if (> n 0)
          (cons (car l) (put (- n 1) (cdr l) x))
          (cons x l)
      )
      (write "Error")
  )
)

(define (setHead l x)
    (cons x l)
)

(define (setTail l x)
    (put (length l) l x)
)

(define (addPaciente l paciente)
    (setTail l paciente)
)
