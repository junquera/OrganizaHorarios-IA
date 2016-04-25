;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;OBTENER EL PACIENTE CON MENOS HORARIOS
;Comprueba si la lista primera de horarios es menor que la segunda, y devuelve en función
(define (obtenerMin l1 l2) (if (< (length l1) (length l2)) l1 l2))
;Obtiene la lista de horarios del paciente introducido
(define (obHorario l) (list-ref l 2))
;Si ha llegado al final de la lista de pacientes, devuelve la lista del ultimo paciente, si no, comprueba el minimo entre todos los pacientes de manera recursiva
(define (listaMinA l) (if (null?(list-tail l 0)) l (obtenerMin((obHorario (list-ref l 0))(listaMin(list-tail l 0)))))) 
(define (listaMin l) (listaMinA l))