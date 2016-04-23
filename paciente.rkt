;; Principio de clase "paciente" para la práctica
;; Estoy mirando como hacer las clases aquí: https://docs.racket-lang.org/reference/createclass.html

(define paciente%
    (class object%
      (super-new)
      (init-field nombre primer_apellido horas)
      (define/public (getNombre) nombre)
      (define/public (getApellido) primer_apellido)
      (define/public (getHoras) horas)))
