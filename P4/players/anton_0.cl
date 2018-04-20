(defpackage :grupo63pareja021732004 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) ; heurística y un alias para el torneo

(in-package grupo63pareja021732004)
(defun heuristica (estado)
  (if (juego-terminado-p estado)
    0
    (reduce '+ (mapcar #'(lambda (hoyo) (get-hoyo-estimacion hoyo estado)) '(0 1 2 3 4 5)))))

(defun get-hoyo-estimacion (hoyo estado)
  (if (robs hoyo estado)
    -5
    (adds-fichas hoyo estado)))

(defun robs (hoyo estado)
  (let ((ends (reaches hoyo estado)))
    (if (< ends 7)
      nil
      (captureable (- ends 7) estado))))

(defun adds-fichas (hoyo estado)
  (let ((ends (reaches hoyo estado))
	(jugador (estado-lado-sgte-jugador estado))
	(contrario (lado-contrario (estado-lado-sgte-jugador estado))))
    (cond 
      ((> ends 6)
       0)
      ((= ends 6)
       1)
      (t 
	(get-fichas (estado-tablero estado) contrario (get-contrario-hoyo ends))))))

(defun captureable (hoyo estado)
  (>= (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) hoyo) 5))

(defun reaches (hoyo estado) 
  (+ (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) hoyo) hoyo))

(defun get-contrario-hoyo (hoyo)
  (- 5 hoyo))


; función de evaluación heurística a implementar
(defvar *alias* 'EL_GATO) ; alias que aparecerá en el ranking
