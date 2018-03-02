;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; EJERCICIO 3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Diseña una función que calcule todas las posibles disposiciones de elementos pertenecientes a
;;;;; N listas de forma que en cada disposición aparezca únicamente un elemento de cada lista
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-list-of-lsts (lstolsts)
  (if (null (rest (rest lstolst)))
      combine-lst-lst (first lstolst) (first (rest lstolst))
    (mapcan (combine-list-of-lsts (rest lstolst)(first lstolst)) )
  )
  )


;;>> (combine-list-of-lsts '(() (+ -) (1 2 3 4)))
;;NIL
;;>> (combine-list-of-lsts '((a b c) () (1 2 3 4)))
;;NIL
;;>> (combine-list-of-lsts '((a b c) (1 2 3 4) ()))
;;NIL
;;>> (combine-list-of-lsts '((1 2 3 4)))
;;((1) (2) (3) (4))
;;>> (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))
