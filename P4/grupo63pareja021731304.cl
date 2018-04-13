(defpackage :grupo63pareja021731304 ; se declara un paquete lisp que usa common-lisp
 (:use :common-lisp :mancala) ; y mancala, y exporta la función de evaluación
 (:export :heuristica :*alias*)) ; heurística y un alias para el torneo
(in-package grupo63pareja021731304)
(defun heuristica (estado)
(if (juego-terminado-p estado)
    (if (< (get-pts (estado-lado-sgte-jugador estado)) (get-pts (lado-contrario (estado-lado-sgte-jugador estado))))
        -10
      100
      )
(- (+(get-pts (estado-lado-sgte-jugador estado)) (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
   (+(get-pts (lado-contrario (estado-lado-sgte-jugador estado)))
        (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)))))
         ; función de evaluación heurística a implementar
(defvar *alias* 'CRISTINA_CIFUENTES) ; alias que aparecerá en el ranking
