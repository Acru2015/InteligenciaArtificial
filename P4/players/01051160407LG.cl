; LGIYW94C6V
; ESPEONZA

(defun mi-f-ev (estado)
 
  (if (juego-terminado-p estado)
      (if (< (get-pts (estado-lado-sgte-jugador estado)) (get-pts (lado-contrario (estado-lado-sgte-jugador estado))))
          -1
        1
          )
    (- (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6)
       (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6) 
        )))
