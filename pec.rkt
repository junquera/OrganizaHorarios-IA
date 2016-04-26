#lang racket

(define HORAS (list 9 10 11 12 13 14 15 16 17 18 19))

;;; ORDENA HORAS
;; Ordena las horas de un paciente
(define (ordenaHoras paciente) (list (list-ref paciente 0) (list-ref paciente 1) (sort (list-ref paciente 2) <) ))

;; Ordena las horas introducidas por los pacientes
(define (ordenaHorasPacientes pacientes)
    (if (= (length pacientes) 0)
        '()
        (cons (ordenaHoras (car pacientes)) (ordenaHorasPacientes (cdr pacientes)))
    )
)
;;; ORDENA HORAS

;;; PARES DE HORAS
(define (empareja n l)
    (if (= (length l) 0)
        null
        (cons (list n (car l)) (empareja n (cdr l)))
    )
)

;; Crea pares de horas
(define (pares l)
    (if (= (length l) 0)
        null
        (append (empareja (car l) (cdr l)) (pares (cdr l)))
    )
)

;; Crea pares de horas de un paciente
(define (emparejaHoras paciente)
    (list (list-ref paciente 0) (list-ref paciente 1) (pares (list-ref paciente 2))))

;; Crea pares de horas para todos los pacientes
(define (pacientesPorPares pacientes)
    (if (= (length pacientes) 0)
        null
        (cons (emparejaHoras (car pacientes)) (pacientesPorPares (cdr pacientes)))
    )
)
;;; PARES DE HORAS

;;; TODO ELIMINA PARES -> ELIMINAR DE CLIENTES, NO DE LISTAS
;; Elimina todas las repeticiones del elemento x de la lista l
(define (borra x l)
    (if (= (length l) 0)
        null
        (if (equal? x (car l))
            (borra x (cdr l))
            (cons (car l) (borra x (cdr l)))
        )
    )
)

(define (eliminaParUsuario par usuario)
    (list (list-ref usuario 0) (list-ref usuario 1) (borra par (list-ref usuario 2)))
)

(define (eliminaParUsuarios par usuarios)
    (if (= (length usuarios) 0)
        null
        (cons (eliminaParUsuario par (car usuarios)) (eliminaParUsuarios par (cdr usuarios)))
    )
)

(define (eliminaParesUsuarios pares usuarios)
    (if (= (length pares) 0)
        usuarios
        (eliminaParesUsuarios (cdr pares) (eliminaParUsuarios (car pares) usuarios))
    )
)

(define (eliminaRepes usuarios)
    (if (= (length usuarios) 0)
        null
        (cons (car usuarios) (eliminaRepes
                    (eliminaParesUsuarios (list-ref (car usuarios) 2) (cdr usuarios))
                )
        )
    )
)
;;; ELIMINA PARES

;;; HORAS
(define (libre hora horas)
    (if (= (length horas) 0)
        #f
        (if (= hora (car horas))
            #t
            (libre hora (cdr horas))
        )
    )
)

(define (borraHora hora horas)
    (borra hora horas)
)

(define (parPosible par horas)
    (and (libre (list-ref par 0) horas) (libre (list-ref par 1) horas))
)

(define (borraParHoras par horas)
    (borraHora (list-ref par 0) (borraHora (list-ref par 1) horas))
)
;;; HORAS

; (list (list "A" "A" (list 1 2 3 4)) (list "B" "B" (list 1 2 4 5)) (list "C" "C" (list 1 3 5 7)))

(define (estructuraPacientes lPacientes)
    (eliminaRepes (pacientesPorPares lPacientes))
)

; (define (voraz usuarios)
; 
; )
