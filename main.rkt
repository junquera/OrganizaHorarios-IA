#lang racket

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

;;; ELIMINA PARES
; borra x l -> (remove* (list x) l)
(define (borra x l)
    (if (= (length l) 0)
        null
        (if (equal? x (car l))
            (borra x (cdr l))
            (cons (car l) (borra x (cdr l)))
        )
    )
)

;; Elimina un par de horas de un usuario
(define (eliminaParUsuario par usuario)
    (list (list-ref usuario 0) (list-ref usuario 1) (borra par (list-ref usuario 2)))
)

;; Elimina un par de horas de todos los usuarios
(define (eliminaParUsuarios par usuarios)
    (if (= (length usuarios) 0)
        null
        (cons (eliminaParUsuario par (car usuarios)) (eliminaParUsuarios par (cdr usuarios)))
    )
)

;; Elimina pares de horas de todos los usuarios
(define (eliminaParesUsuarios pares usuarios)
    (if (= (length pares) 0)
        usuarios
        (eliminaParesUsuarios (cdr pares) (eliminaParUsuarios (car pares) usuarios))
    )
)

;; Diseñamos este método para hacer una criba y eliminar pares de horas repetidas
;; entre pacientes. Pero al final no lo hemos utilizado porque elimina algunos
;; resultados válidos.
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
; libre -> (memq hora horas)
(define (libre hora horas)
    (if (= (length horas) 0)
        #f
        (if (= hora (car horas))
            #t
            (libre hora (cdr horas))
        )
    )
)
;; Borra una hora de la lista de horas disponibles
(define (borraHora hora horas)
    (borra hora horas)
)

;; Comprueba si un par de horas encaja en las horas disponibles
(define (parPosible par horas)
    (and (libre (list-ref par 0) horas) (libre (list-ref par 1) horas))
)

;; Borra un par de horas de la lista de horas disponibles
(define (borraParHoras par horas)
    (borraHora (list-ref par 0) (borraHora (list-ref par 1) horas))
)
;;; HORAS


;;; ORDENAR DE MENOS A MAS HORARIOS
(define (menosHoras l1 l2)
    (if (< (length (list-ref l1 2)) (length (list-ref l2 2)))
        l1
        l2
    )
)

(define (obtenerMenorAux menor pacientes)
    (if (= (length pacientes) 0)
        menor
        (obtenerMenorAux (menosHoras menor (car pacientes)) (cdr pacientes))
    )
)

(define (obtenerMenor pacientes)
    (obtenerMenorAux (car pacientes) (cdr pacientes))
)

;; Pone primero el elemento indicado y lo concatena a la cola sin ese elemento
(define (ordenaPrimerElemento cabeza cola)
    (cons cabeza (ordenaPacientesNHoras (borra cabeza cola)))
)
;; Ordena los pacientes en función del número de pares de horas que tienen
;; disponibles, poniendo primero los que tienen menos disponibilidad
(define (ordenaPacientesNHoras pacientes)
    (if (= (length pacientes) 0)
        '()
        (ordenaPrimerElemento (obtenerMenor pacientes) pacientes)
    )
)
;;; ORDENAR DE MENOS A MAS HORARIOS

;;; ANÁLISIS Y RESULTADOS
;; Convierte la lista de horas del paciente en una lista de pares de horas y
;; ordena los pacientes en función del menor número de horas
(define (estructuraPacientes lPacientes)
    ;(eliminaRepes
        (ordenaPacientesNHoras (pacientesPorPares (ordenaHorasPacientes lPacientes)))
    ;)
)

;; Borra un par de horas de un paciente
(define (borraParPaciente par paciente)
    (list (list-ref paciente 0) (list-ref paciente 1) (borra par (list-ref paciente 2)))
)

;; Método voraz (búsqueda de escalada) que devuelve las citas de los pacientes
 (define (voraz pacientes horas)
    (if (= (length pacientes) 0)
        '()
        ;; Si al paciente no le quedan horas
        (if (= (length (list-ref (car pacientes) 2)) 0)
            ;; Lo descartamos y pasamos al siguiente
            (voraz (cdr pacientes) horas)
            ;; Si no, vemos si su primer par de horas es seleccionable
            (if (parPosible (car (list-ref (car pacientes) 2)) horas)
                ;; Si lo es, lo cogemos
                (cons (list (list-ref (car pacientes) 0) (list-ref (car pacientes) 1) (car (list-ref (car pacientes) 2))) (voraz (cdr pacientes) (borraParHoras (car (list-ref (car pacientes) 2)) horas)) )
                ;; Si no, probamos con su siguiente par de horas de forma recursiva
                (voraz (cons (borraParPaciente (car (list-ref (car pacientes) 2)) (car pacientes)) (cdr pacientes)) horas)
            )
        )
    )
 )
;;; ANÁLISIS Y RESULTADOS

;;; INTERFAZ GRÁFICA
;; Elige qué paciente entra antes en función de la hora de su cita
(define (masTemprano menor pacientes)
    (if (= (length pacientes) 0)
        menor
        (if (< (list-ref (car pacientes) 2) (list-ref menor 2))
            (masTemprano (car pacientes) (cdr pacientes))
            (masTemprano menor (cdr pacientes))
        )
    )
)

(define (ordenaPorHoraAux temprano pacientes)
    (cons temprano (ordenaPorHora (borra temprano pacientes)))
)

;; Ordena los pacientes en función de su hora
(define (ordenaPorHora pacientes)
    (if (= (length pacientes) 0)
        null
        (ordenaPorHoraAux (masTemprano (car pacientes) (cdr pacientes)) pacientes)
    )
)

;; Separa un paciente en dos para obtener la hora de cada cita
(define (horasDePaciente paciente)
    (if (= (length (list-ref paciente 2)) 0)
        null
        (cons (list (list-ref paciente 0) (list-ref paciente 1) (car (list-ref paciente 2)))
        (horasDePaciente (list (list-ref paciente 0) (list-ref paciente 1) (cdr (list-ref paciente 2)))))
    )
)

;; Convierte la lista final de pacientes en una lista de horas
(define (pacienteHora pacientes)
    (if (= (length pacientes) 0)
        null
        (append (horasDePaciente (car pacientes)) (pacienteHora (cdr pacientes)))
    )
)
;; Divide los pacientes en listas de nombre, apellido y hora
(define (imprimeResultado listaHoras)
    (ordenaPorHora (pacienteHora listaHoras))
)
;;; INTERFAZ GRÁFICA

;; Recibe la lista de pacientes y los estructura para sacar el resultado ordenado
(define (main listaPacientes)
    (imprimeResultado (voraz (estructuraPacientes listaPacientes) (list 9 10 11 12 13 14 15 16 17 18)))
)

;;; CASOS DE PRUEBA
 (main (list (list "A" "A" (list 9 10 11 12 13)) (list "B" "B" (list 14 12)) (list "C" "C" (list 13 15 17)) (list "D" "D" (list 12 18 15)) (list "E" "E" (list 9 10 13 18 15))))
 (main (list (list "A" "A" (list 9 10 11 12)) (list "B" "B" (list 9 10 11 12))))
 (main (list (list "A" "A" (list 9 10 11 12)) (list "B" "B" (list 9 10))))
