#lang racket
;; Principio de clase "paciente" para la práctica
;; Estoy mirando como hacer las clases aquí: https://docs.racket-lang.org/reference/createclass.html

(define paciente%
    (class object%
      (super-new)
      (init-field nombre apellido horas)
      (define/public (getNombre) nombre)
      (define/public (getApellido) apellido)
      (define/public (getHoras) horas)))

;; getNombre --> (send objetoInstanciado getNombre)
