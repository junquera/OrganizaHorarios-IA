#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;OBTENER EL PACIENTE CON MENOS HORARIOS
;Comprueba si la lista primera de horarios es menor que la segunda, y devuelve en función
(define (obtenerMin l1 l2) (if (< (length l1) (length l2)) l1 l2))
;Obtiene la lista de horarios del paciente introducido
(define (obHorario l) (list-ref l 2))
;Si ha llegado al final de la lista de pacientes, devuelve la lista del ultimo paciente, si no, comprueba el minimo entre todos los pacientes de manera recursiva
(define (listaMinA l) (if (null?(list-tail l 1)) l (obtenerMin((obHorario (car l))(listaMin(list-tail l 1)))))) 
(define (listaMin l) (listaMinA l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;OBTENER EL PACIENTE CON LOS HORARIOS AJUSTADOS
;Obtiene el primero horario de la lista de horarios pasada
(define (obPrimHora l) (list-ref l 0))
;Crea una lista axuiliar para ser tratada
(define (listAuxC) '())
;Si el primer horario se encuentra en la lista auxiliar, se elimina y se va al siguiente, sino, se añade a la lista auxiliar y se va al siguiente NO SE HA CONTROLADO TODAVIA QUE PARE CUANDO EL NÚMERO DE HORARIO QUE TIENE ES 2
;YA QUE SOLO PUEDE TENER 2 O MENOS, Y EN CASO DE MENOS DE 2 AVISAR EN LA SALIDA
(define (comprobarAux l listAuxContar) (if (memq (obPrimHora l) listAuxContar) (comprobarAux(remove (obPrimHora l) l) listAuxContar) (comprobarAux(list-tail l 1)(append (obPrimHora l)(listAuxContar)))))
;para que funcione comprobar, he supuesto que se le pasa como l la lista de horarios del paciente, y no el paciente en si
(define (comprobar l) (comprobarAux l (listAuxC)))
(define (listSalida) '())
;Se crea el método añadirListaS para unir la lista del paciente después de haber comprobado su horario
(define (añadirListaS l) (append (comprobar (list-ref l 2)) listSalida)) 

;Después de realizarlo para un paciente se pretende llamar recursivamente al método de listaMin que eliminaría al elemento escogido recursivamente como el menor, sin antes añadirlo
;a la lista de salida para seguir con los siguientes como por ejemplo
;(define (ordenar l) (

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EJEMPLOS DE ENTRADAS
(listaMin'('( "asdsad" "asdsadas" 1234) '( "asdsad" "asdsadsa" 12) '("asdqweqw" "saccerf" 123412)))
(comprobar (list 4 5 6 7))