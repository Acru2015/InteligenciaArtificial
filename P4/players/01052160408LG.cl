; LGIYW94C6V
; ESPEONZA2

(defun mi-f-ev (estado)
 
  (if (juego-terminado-p estado)
      (if (< (get-pts (estado-lado-sgte-jugador estado)) (get-pts (lado-contrario (estado-lado-sgte-jugador estado))))
          -1
        1
        )
    (- (get-pts (estado-lado-sgte-jugador estado))
     (get-pts (lado-contrario (estado-lado-sgte-jugador estado))) 
       )))