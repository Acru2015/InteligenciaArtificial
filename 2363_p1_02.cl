(defun check-valid (v) 
  (and (not (equal nil v)) (not (every (lambda (elem) (= elem 0)) v))))

#|
(write (check-valid '(1 2 3)))
(write-line "")
(write (check-valid '(0 2 3)))
(write-line "")
(write (check-valid '(0 0 0)))
(write-line "")
(write (check-valid nil))
(write-line "")
|#

(defun array-mult (x y)
  (if (null x)
    0 
    (+ (* (first x) (first y)) (array-mult (rest x) (rest y)))))

(defun array_square (x) (array-mult x x))

(defun sc-rec (x y)
  (if (and (check-valid x) (check-valid y))
    (/ (array-mult x y) (* (sqrt (array_square x)) (sqrt(array_square y))))))

#|
(write-line "Sc rec")
(write (sc-rec '(3 4) '(3 4)))
(write-line "")
(write (sc-rec '(-1 -1) '(1 1)))
(write-line "")
(write (sc-rec '(0 1) '(1 0)))
(write-line "")
(write (sc-rec '(0 0) '(1 0)))
(write-line "")
(write (sc-rec '(5 7) '(4 3)))
(write-line "")
|#

(defun map-mult (x y)
  (reduce '+ (mapcar '* x y)))

(defun map-square (x)
  (map-mult x x))

(defun sc-mapcar (x y)
  (if (and (check-valid x) (check-valid y))
    (/ (map-mult x y) (* (sqrt (map-square x)) (sqrt (map-square y))))))

#|
(write-line "Sc map")
(write (sc-mapcar '(3 4) '(3 4)))
(write-line "")
(write (sc-mapcar  '(-1 -1) '(1 1)))
(write-line "")
(write (sc-mapcar '(0 1) '(1 0)))
(write-line "")
|#

(defun get-sims (x vs)
  (mapcar (lambda (v-n) (sc-mapcar x v-n)) vs))

#|
(write (get-sims '(1 2 3) '((1 2 3) (0 1 0) (4 5 6))))
(write-line "")

(write-line "Simularities")
(write (get-sims '(1 2 3) '((1 2 3) (0 1 0) (4 5 6))))
(write-line "")
|#

(defun filter (x conf)
  (remove-if (lambda (elem) (< elem conf)) x))

#|
(write (filter '(0 5 10 3 4) 4))
(write-line "")

(write-line "Filter")
(write (filter '(0 5 10 3 4) 4))
(write-line "")
|#

(defun get-filtered-sims (x vs conf)
  (filter (get-sims x vs) conf))

(defun sc-conf (cat vs conf)
  (sort (get-filtered-sims cat vs conf) '<))

#|
(write-line "Sc conf")
(write (sc-conf '(1 2 3) '((1 2 3) (0 1 0) (4 5 6)) 0.7))
(write-line "")
|#

(defun get-sim-couples-with-func (text categories func)
  (mapcar (lambda (category) (cons (first category) (funcall func text (rest category)))) categories))

#|
(write-line "Sim couples:")
(write (get-sim-couples-with-func '(1 2 3 4) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")
|#

(defun get-max-sims (text categories func)
  (first (sort (get-sim-couples-with-func text categories func) (lambda (sim1 sim2) (> (cdr sim1) (cdr sim2))))))

#|
(write-line "Max sim couples:")
(write (get-max-sims '(1 2 3 4) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")
|#

(defun sc-classifier (cats texts func)
  (mapcar (lambda (text) (get-max-sims (rest text) cats func)) texts))

#|
(write-line "get classifiers")
(write (sc-classifier '((0 1 2 3 4) (1 3 2 2 0) (2 3 5 8 0)) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")
|#


(setf cats0 '((1 43 23 12) (2 33 54 24)))
(setf texts0 '((1 3 22 134) (2 43 26 58)))

(setf cats1 '((1 43)  (54 24)))
(setf texts1 '((1 31) (26 58)))

(setf cats2 '((1 43 23 12 44) (2 33 54 24 93) (3 35 33 22 12) (4 55 21 12 34) (5 90 22 48 9 8)))
(setf texts2 '((1 3 22 134 388) (2 43 26 58 57) (3 4 6 2 0) (4 56 33 00 11) (5 38 13 92 29)))

#|
(write-line "0")
(write (time (sc-classifier cats0 texts0 #'sc-rec)))
(write-line "")
(write (time (sc-classifier cats0 texts0 #'sc-mapcar)))
(write-line "")
(write-line "1")
(write (time (sc-classifier cats1 texts1 #'sc-rec)))
(write-line "")
(write (time (sc-classifier cats1 texts1 #'sc-mapcar)))
(write-line "")
(write-line "2")
(write (time (sc-classifier cats2 texts2 #'sc-rec)))
(write-line "")
(write (time (sc-classifier cats2 texts2 #'sc-mapcar)))
(write-line "")
|#

(defun root-in-intervall (f a b)
  (< (* (funcall f a) (funcall f b)) 0))

(defun below-limit (a b limit)
  (< (- b a) limit))

(defun middle-x (a b)
  (/ (+ a b) 2))

(defun in-lower-interval (f a x)
  (< (* (funcall f a) (funcall f x)) 0))

(defun bisect (f a b tol)
  (when (root-in-intervall f a b)
    (let ((x (middle-x a b)))
      (if (or (= x 0) (below-limit a b tol))
	x
	(if (in-lower-interval f a x)
	  (bisect f a x tol)
	  (bisect f x b tol))))))

#|
(write-line "Bisect")
(write (bisect #'(lambda (x) (sin (* 6.26 x))) 0.1 0.7 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.26 x))) 0.0 0.7 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.28 x))) 1.1 1.5 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.28 x))) 1.1 2.1 0.001))
(write-line "")
|#

(defun get-couples (lst)
  (mapcar 'cons lst (rest lst)))

(defun allroot (f lst tol)
  (mapcan #'(lambda (interval) (list (bisect f (car interval) (cdr interval) tol))) (get-couples lst)))

#|
(write-line "All roots")
(write (allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001))
(write-line "")
|#

(defun get-step-size (a b N) 
  (/ (- b a) (expt 2 N)))

#|
(write-line "Step size")
(write (get-step-size 0.1 2.25 2))
(write-line "")
|#

(defun create-base-list (a N)
  (make-list (+ 1 N) :initial-element a))

(defun number-sequence (start end)
  (loop for n from start to end collect n))

#|
(write-line "Number sequence")
(write (number-sequence 0 4))
(write-line "")
|#

(defun create-base-intervals (step-size no-steps)
(mapcar #'(lambda (x) (* x step-size)) (number-sequence 0 no-steps)))

(defun create-interval-list (a b N)
  (let ((step-size (get-step-size a b N))
	(no-steps (expt 2 N)))
    (mapcar '+ (create-base-list a no-steps) (create-base-intervals step-size no-steps))))
#|
(write-line "Interval list")
(write (create-interval-list 0.1 2.25 2))
(write-line "")
|#

(defun allind (f a b N tol) 
  (allroot f (create-interval-list a b N) tol))

#|
(write-line "Allind")
(write (allind #'(lambda(x) (sin (* 6.28 x ))) 0.1 2.25 1 0.0001))
(write-line "")
(write (allind #'(lambda(x) (sin (* 6.28 x ))) 0.1 2.25 2 0.0001))
(write-line "")
|#

(defun combine-elt-lst (-elt lst)
  (mapcar #'(lambda(elem) (append (if (listp -elt) -elt (list -elt)) (list elem))) lst))

#|
(write-line "combine-elt-lst")
(write (combine-elt-lst 'a nil))
(write-line "")
(write (combine-elt-lst 'a '(1 2 3)))
(write-line "")
(write (combine-elt-lst '(a b c) '(1 2 3)))
(write-line "")
|#

(defun combine-lst-lst (lst1 lst2)
  (mapcan #'(lambda (elem) (combine-elt-lst elem lst2)) lst1))

#|
(write-line "combine-lst-lst")
(write (combine-lst-lst nil nil))
(write-line "")
(write (combine-lst-lst '(a b c) nil))
(write-line "")
(write (combine-lst-lst NIL '(a b c)))
(write-line "")
(write (combine-lst-lst '(a b c) '(1 2)))
(write-line "")
|#

;(defun combine-list-of-lsts (lstolsts)
;  (reduce #'combine-lst-lst lstolsts))

(defun rec-combine (combined to-combine)
  (if (null to-combine)
    combined
    (rec-combine (combine-lst-lst combined (first to-combine)) (rest to-combine))))

(defun combine-list-of-lsts (lstolsts)
  (rec-combine (first lstolsts) (rest lstolsts)))

#|
(defun combine-list-of-lsts (lstolst)
  (if (null (rest (rest lstolst)))
    (combine-lst-lst (car lstolst) (car (cdr lstolst)))
    (mapcan (combine-list-of-lsts (rest lstolst)) (first lstolst))))
|#

;(defun combine-list-of-lsts (lstolsts)
;  (mapcar #'(lambda (elem) (combine-elt-lst elem (second lstolsts))) (first lstolsts)))

#|
(write-line "combine-list-of-lsts")
(write (combine-list-of-lsts '(() (+ -) (1 2 3 4))))
(write-line "")
(write (combine-list-of-lsts '((a b c) () (1 2 3 4))))
(write-line "")
(write (combine-list-of-lsts '((a b c) (1 2 3 4) ())))
(write-line "")
(write (combine-list-of-lsts '((1 2 3 4))))
(write-line "")
(write (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4))))
(write-line "")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '¬)
(defconstant f nil)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p   x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  ;;
  ;; 4.1.1 Completa el codigo
  ;;
  (and (atom x) (not (truth-value-p x)) (not (connector-p x))))

;; EJEMPLOS:
#|
(print "positive-literal-p")
(print (positive-literal-p 'p))
;; evalua a T
(print (positive-literal-p T))
(print (positive-literal-p NIL))
(print (positive-literal-p '¬))
(print (positive-literal-p '=>))
(print (positive-literal-p '(p)))
(print (positive-literal-p '(¬ p)))
(print (positive-literal-p '(¬ (v p q))))
;; evaluan a NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
  ;;
  ;; 4.1.2 Completa el codigo
  ;;
  (and (listp x) (unary-connector-p (car x)) (positive-literal-p (cadr x))))

;; EJEMPLOS:
#|
(print "negative-literal-p")
(print (negative-literal-p '(¬ p)))        ; T
(print (negative-literal-p NIL))           ; NIL
(print (negative-literal-p '¬))            ; NIL
(print (negative-literal-p '=>))           ; NIL
(print (negative-literal-p '(p)))          ; NIL
(print (negative-literal-p '((¬ p))))      ; NIL
(print (negative-literal-p '(¬ T)))        ; NIL
(print (negative-literal-p '(¬ NIL)))      ; NIL
(print (negative-literal-p '(¬ =>)))       ; NIL
(print (negative-literal-p 'p))            ; NIL
(print (negative-literal-p '((¬ p))))      ; NIL
(print (negative-literal-p '(¬ (v p q))))  ; NIL
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  ;;
  ;; 4.1.3 Completa el codigo
  ;;
  (or (positive-literal-p x) (negative-literal-p x)))

;; EJEMPLOS:
#|
(print "literal-p")
(print (literal-p 'p))             
(print (literal-p '(¬ p)))      
;;; evaluan a T
(print (literal-p '(p)))
(print (literal-p '(¬ (v p q))))
;;; evaluan a  NIL
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
	(and (listp x)         ;; En caso de que no sea un literal debe ser una lista
	     (let ((connector (first x))
		   (rest_1    (rest  x)))
	       (cond
		 ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
		  (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
		       (wff-prefix-p (first rest_1)))) 
		 ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
		  (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
		    (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
			 (wff-prefix-p (first rest_1))
			 (wff-prefix-p (first rest_2)))))               
		 ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
		  (or (null rest_1)              ;; conjuncion o disyuncion vacias
		      (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
			   (let ((rest_2 (rest rest_1)))
			     (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
				 (wff-prefix-p (cons connector rest_2)))))))	
		 (t NIL)))))))                   ;; No es FBF en formato prefijo 
;;
;; EJEMPLOS:
#|
(print "wff prefix true")
(print (wff-prefix-p '(v)))
(print (wff-prefix-p '(^)))
(print (wff-prefix-p '(v A)))
(print (wff-prefix-p '(^ (¬ B))))
(print (wff-prefix-p '(v A (¬ B))))
(print (wff-prefix-p '(v (¬ B) A )))
(print (wff-prefix-p '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)))
;;; evaluan a T
(print "wff prefix false")
(print (wff-prefix-p 'NIL))
(print (wff-prefix-p '(¬)))
(print (wff-prefix-p '(=>)))
(print (wff-prefix-p '(<=>)))
(print (wff-prefix-p '(^ (V P (=> A ( B ^ (¬ C) ^ D))) (^ (<=> P (¬ Q)) P) E)))
;;; evaluan a NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato infijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato infijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-p (x)
  ;;
  ;; 4.1.4 Completa el codigo
  ;;
  (unless (null x)             ;; NIL no es FBF en formato infijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato infijo 
	(and (listp x)         ;; En caso de que no sea un literal debe ser una lista
	     (cond
	       ((unary-connector-p (first x))  ;; Si el primer elemento es un connector unario
		(and (null (rest (rest x)))      ;; deberia tener la estructura (<conector> FBF)
		     (wff-infix-p (second x)))) 
	       ((binary-connector-p (second x)) ;; Si el primer elemento es un conector binario
		;; deberia tener la estructura 
		(and (null (rest (rest (rest x))))    ;; (FBF1 <conector> FBF2)
		     (wff-infix-p (first x))
		     (wff-infix-p (third x))))               
	       ((n-ary-connector-p (first x))	;; conjuncion o disyuncion vacias
		(null (rest x)))
	       ((n-ary-connector-p (second x))  ;; Si el primer elemento es un conector enario
		(and (or (null (fourth x)) (equal (second x) (fourth x))) ;; solo pueden ser connectores mismas
		     (wff-infix-p (first x)) ;; tienen que ser FBF los operandos 
		     (wff-infix-p (first (rest (rest x))))))
	       (t NIL))))))                  ;; No es FBF en formato prefijo 

;;
;; EJEMPLOS:
;;
#|
(print "Wff infix true")
(print (wff-infix-p 'a)) 						; T
(print (wff-infix-p '(^))) 					; T  ;; por convencion
(print (wff-infix-p '(v))) 					; T  ;; por convencion
(print (wff-infix-p '(¬ c))) 						; T
(print (wff-infix-p '(A ^ (v)))) 			      ; T  
(print (wff-infix-p '(A ^ (p v q)))) 			      ; T  
(print (wff-infix-p '( a ^ b ^ (p v q) ^ (¬ r) ^ s)))  	; T 
(print (wff-infix-p '(A => B))) 				; T
(print (wff-infix-p '(A => (B <=> C)))) 			; T
(print (wff-infix-p '( B => (A ^ C ^ D)))) 			; T   
(print (wff-infix-p '( B => (A ^ C)))) 			; T 
(print (wff-infix-p '( B ^ (A ^ C)))) 			; T 
(print (wff-infix-p '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e)))  ; T 
(print "Wff infix false")
(print (wff-infix-p nil)) 					; NIL
(print (wff-infix-p '(a ^))) 					; NIL
(print (wff-infix-p '(^ a))) 					; NIL
(print (wff-infix-p '(A ^ v))) 			      ; NIL 
(print (wff-infix-p '(c ¬))) 						; NIL
(print (wff-infix-p '(c ¬ c))) 						; NIL
(print (wff-infix-p '(a))) 					; NIL
(print (wff-infix-p '((a)))) 				      ; NIL
(print (wff-infix-p '((a) b)))   			      ; NIL
(print (wff-infix-p '(^ a b q (¬ r) s)))  		      ; NIL 
(print (wff-infix-p '( B => A C))) 			      ; NIL   
(print (wff-infix-p '( => A))) 				      ; NIL   
(print (wff-infix-p '(A =>))) 				      ; NIL   
(print (wff-infix-p '(A => B <=> C))) 		      ; NIL
(print (wff-infix-p '( B => (A ^ C v D)))) 		      ; NIL   
(print (wff-infix-p '( B ^ C v D ))) 			      ; NIL 
(print (wff-infix-p '((p v (a => e (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e))); NIL 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
      wff
      (let ((connector      (first wff))
	    (elements-wff (rest wff)))
	(cond
	  ((unary-connector-p connector) 
	   (list connector (prefix-to-infix (second wff))))
	  ((binary-connector-p connector) 
	   (list (prefix-to-infix (second wff))
		 connector
		 (prefix-to-infix (third wff))))
	  ((n-ary-connector-p connector) 
	   (cond 
	     ((null elements-wff)        ;;; conjuncion o disyuncion vacias. 
	      wff)                       ;;; por convencion, se acepta como fbf en formato infijo
	     ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
	      (prefix-to-infix (car elements-wff)))  
	     (t (cons (prefix-to-infix (first elements-wff)) 
		      (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) 
			      (rest elements-wff))))))
	  (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;;  EJEMPLOS:
;;
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (¬ a)))    ; (¬ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;;; ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d))))) ; (P V (A => (B ^ (¬ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))     ; (((P <=> (¬ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (¬ p) q (¬ r) (¬ s)))       ; ((¬ P) V Q V (¬ R) V (¬ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-connector (connector -list)
  (remove-if #'(lambda (elem) (equal connector elem)) -list))

#|
(print (remove-connector 'v '(a v p)))
(print (remove-connector 'v '(a v p v b)))
|#

(defun infix-to-prefix (wff)
  ;;
  ;; 4.1.5 Completa el codigo
  ;;
  (when (wff-infix-p wff)
    (if (literal-p wff)
      wff
      (cond
	((unary-connector-p (first wff)) 
	 (list (first wff) (infix-to-prefix (second wff))))
	((binary-connector-p (second wff)) 
	 (list (second wff)
	       (infix-to-prefix (first wff))
	       (infix-to-prefix (third wff))))
	((n-ary-connector-p (first wff)) 
	 (when (null (rest wff)		;;; conjuncion o disyuncion vacias. 
		     wff)))
	((n-ary-connector-p (second wff))
	 (cons (second wff)
	       (let ((sub-wffs (cons (first wff) (remove-connector (second wff) (rest (rest wff))))))
		 (mapcar #'(lambda (elem) (infix-to-prefix elem)) sub-wffs))))
	(t NIL))))) ;; no deberia llegar a este paso nunca

;;
;; EJEMPLOS
;;
#|
(print (infix-to-prefix nil))      ;; NIL
(print (infix-to-prefix 'a))       ;; a
(print (infix-to-prefix '((a))))   ;; NIL
(print (infix-to-prefix '(a)))     ;; NIL
(print (infix-to-prefix '(((a))))) ;; NIL
(print (infix-to-prefix '(a v p)))
(print (infix-to-prefix '(a v p v b)))
(print (infix-to-prefix '(a v p v b v x)))
(print (infix-to-prefix '(a v p v b v x v y)))
(print (equal 
	 (prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)) ) 
	 '((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)))


(print (equal (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))
	      '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)))

(print (equal (infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
	      '(¬ (V (¬ P) Q (¬ R) (¬ S)))))


(print (equal (infix-to-prefix
		(prefix-to-infix
		  '(V (¬ P) Q (¬ R) (¬ S))))
	      '(V (¬ P) Q (¬ R) (¬ S))))

(print (equal (infix-to-prefix
		(prefix-to-infix
		  '(¬ (V (¬ P) Q (¬ R) (¬ S)))))
	      '(¬ (V (¬ P) Q (¬ R) (¬ S)))))


(print (equal (infix-to-prefix 'a) 'A))
(print (equal (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e)) 
	      '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)))

(print (equal (infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
	      '(¬ (V (¬ P) Q (¬ R) (¬ S)))))

(print (equal (infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d)))))) '(v p (=> a (^ b (¬ c) d)))))
(print (equal (infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e)))  '(^ (^ (<=> p (¬ q)) p ) e)))
(print (equal (infix-to-prefix (prefix-to-infix '( v (¬ p) q (¬ r) (¬ s))))  '( v (¬ p) q (¬ r) (¬ s))))
;;;

(print (equal (infix-to-prefix '(p v (a => (b ^ (¬ c) ^ d)))) '(V P (=> A (^ B (¬ C) D)))))
(print (equal (infix-to-prefix '(((P <=> (¬ Q)) ^ P) ^ E))  '(^ (^ (<=> P (¬ Q)) P) E)))
(print (equal (infix-to-prefix '((¬ P) V Q V (¬ R) V (¬ S))) '(V (¬ P) Q (¬ R) (¬ S))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p (wff)
  ;;
  ;; 4.1.6 Completa el codigo
  ;;
  (and (listp wff)
       (eql (first wff) +or+)
       (reduce #'(lambda (prev elem) (and (literal-p elem) prev)) (rest wff) :initial-value t)))

;;
;; EJEMPLOS:
;;
#|
(print "Clause p")
(print (clause-p '(v)))             ; T
(print (clause-p '(v p)))           ; T
(print (clause-p '(v (¬ r))))       ; T
(print (clause-p '(v p q (¬ r) s))) ; T
(print (clause-p NIL))                    ; NIL
(print (clause-p 'p))                     ; NIL
(print (clause-p '(¬ p)))                 ; NIL
(print (clause-p NIL))                    ; NIL
(print (clause-p '(p)))                   ; NIL
(print (clause-p '((¬ p))))               ; NIL
(print (clause-p '(^ a b q (¬ r) s)))     ; NIL
(print (clause-p '(v (^ a b) q (¬ r) s))) ; NIL
(print (clause-p '(¬ (v p q))))           ; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-p (wff)
  ;;
  ;; 4.1.7 Completa el codigo
  ;;
  (and (listp wff)
       (eql (first wff) +and+)
       (reduce #'(lambda (prev elem) (and (clause-p elem) prev)) (rest wff) :initial-value t)))

;;
;; EJEMPLOS:
;;
#|
(print (cnf-p '(^ (v a  b c) (v q r) (v (¬ r) s) (v a b)))) ; T
(print (cnf-p '(^ (v a  b (¬ c)) )))                        ; T
(print (cnf-p '(^ )))                                       ; T
(print (cnf-p '(^(v ))))                                    ; T
(print (cnf-p '(¬ p)))                                      ; NIL
(print (cnf-p '(^ a b q (¬ r) s)))                          ; NIL
(print (cnf-p '(^ (v a b) q (v (¬ r) s) a b)))              ; NIL
(print (cnf-p '(v p q (¬ r) s)))                            ; NIL
(print (cnf-p '(^ (v a b) q (v (¬ r) s) a b)))              ; NIL
(print (cnf-p '(^ p)))                                      ; NIL
(print (cnf-p '(v )))                                       ; NIL
(print (cnf-p NIL))                                         ; NIL
(print (cnf-p '((¬ p))))                                    ; NIL
(print (cnf-p '(p)))                                        ; NIL
(print (cnf-p '(^ (p))))                                    ; NIL
(print (cnf-p '((p))))                                      ; NIL
(print (cnf-p '(^ a b q (r) s)))                            ; NIL
(print (cnf-p '(^ (v a  (v b c)) (v q r) (v (¬ r) s) a b))) ; NIL
(print (cnf-p '(^ (v a (^ b c)) (^ q r) (v (¬ r) s) a b)))  ; NIL
(print (cnf-p '(¬ (v p q))))                                ; NIL
(print (cnf-p '(v p q (r) s)))                              ; NIL 
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-biconditional (wff)
  (if (or (null wff) (literal-p wff))
    wff
    (let ((connector (first wff)))
      (if (eq connector +bicond+)			;; cuando estas un bicond
	(let ((wff1 (eliminate-biconditional (second wff))) ;; eliminacion en fnc 1
	      (wff2 (eliminate-biconditional (third  wff)))) ;; eliminacion en fnc 2
	  (list +and+ 						;; transformacion
		(list +cond+ wff1 wff2)
		(list +cond+ wff2 wff1)))
	(cons connector 				;; cuando no estas
	      (mapcar #'eliminate-biconditional (rest wff))))))) ;; eleminacion en el resto

;;
;; EJEMPLOS:
;;
#|
(print (eliminate-biconditional '(<=> p  (v q s p) )))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(print (eliminate-biconditional '(<=>  (<=> p  q) (^ s (¬ q)))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (¬ Q)))
;;      (=> (^ S (¬ Q)) (^ (=> P Q) (=> Q P))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  
  ;;
  ;; 4.2.2 Completa el codigo
  (if (or (null wff) (literal-p wff))
    wff
    (let ((connector (first wff)))
      (if (eq connector +cond+)			
	(let ((wff1 (eliminate-conditional (second wff)))
	      (wff2 (eliminate-conditional (third  wff)))) 
	  (list +or+ 				
		(list +not+ wff1)
		wff2))
	(cons connector 		
	      (mapcar #'eliminate-conditional (rest wff)))))))

;;
;; EJEMPLOS:
;;
#|
(print (equal (eliminate-conditional '(=> p q)) '(V (¬ P) Q)))
(print (equal (eliminate-conditional '(=> p (v q s p))) '(V (¬ P) (V Q S P))))
(print (equal (eliminate-conditional '(=> (=> (¬ p) q) (^ s (¬ q)))) '(V (¬ (V (¬ (¬ P)) Q)) (^ S (¬ Q)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNF equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce-scope-of-negation (wff)
  ;;
  ;; 4.2.3 Completa el codigo
  ;;
  (if (or (null wff) (literal-p wff))
    wff
    (if (equal (first wff) +not+)
      (let ((connector (first (first (rest wff))))
	    (tail (rest (first (rest wff)))))
	(if (n-ary-connector-p connector)
	  (cons
	    (exchange-and-or connector)
	    (mapcar #'(lambda (elem) (reduce-scope-of-negation (list +not+ elem))) tail))
	  (mapcan #'(lambda (elem) (reduce-scope-of-negation elem)) tail)))
      (cons (first wff) (mapcar #'(lambda (elem) (reduce-scope-of-negation elem)) (rest wff))))))

(defun exchange-and-or (connector)
  (cond
    ((eq connector +and+) +or+)    
    ((eq connector +or+) +and+)
    (t connector)))

;;
;;  EJEMPLOS:
;;
#|
(print "Reduce scope of negation")
(print (reduce-scope-of-negation 'r))
(print (reduce-scope-of-negation '(¬ r)))
(print (reduce-scope-of-negation '(v p)))
(print (reduce-scope-of-negation '(v p r)))
(print (reduce-scope-of-negation '(¬ (v p))))
(print (reduce-scope-of-negation '(¬ (v p r))))
(print (reduce-scope-of-negation '(¬ (v p (¬ q) r))))
(print (equal (reduce-scope-of-negation '(¬ (v p (¬ q) r))) 
	      '(^ (¬ P) Q (¬ R))))
(print (equal (reduce-scope-of-negation '(¬ (^ p (¬ q) (v  r s (¬ a))))) 
	      '(V (¬ P) Q (^ (¬ R) (¬ S) A))))
(print (equal (reduce-scope-of-negation '(¬ (v fbf1 fbf2 fbf3))) '(^ (¬ fbf1) (¬ fbf2) (¬ fbf3))))
(print (reduce-scope-of-negation '(¬ (v fbf1 fbf2 fbf3))))
(print (equal (reduce-scope-of-negation '(¬ (^ fbf1 fbf2 fbf3))) '(v (¬ fbf1) (¬ fbf2) (¬ fbf3))))
(print (reduce-scope-of-negation '(¬ (^ fbf1 fbf2 fbf3))))
(print (reduce-scope-of-negation '(v fbf1 fbf3)))
(print (reduce-scope-of-negation '(¬ (v fbf1 fbf3))))
(print (reduce-scope-of-negation '(^ (¬ (v fbf1 fbf3)) fbf2 fbf3)))
(print (reduce-scope-of-negation '(¬ (^ (¬ (v fbf1 fbf3)) fbf2 fbf3))))
(print (reduce-scope-of-negation '(¬ (¬ c1))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst)
  (if (null lst)
    (list (list elt))
    (mapcar #'(lambda (x) (cons elt x)) lst)))
#|
(print "combine elt lst")
(print "x (3 4)")
(print (combine-elt-lst 'x '(3 4)))
|#

(defun exchange-NF (nf)			;;simplemente intercambia connector
  (if (or (null nf) (literal-p nf)) 
    nf
    (let ((connector (first nf)))
      (cons (exchange-and-or connector) ;;empieza con el connector intercambio
	    (mapcar #'(lambda (x)	;;tira el connector por dentro
			(cons connector x))
		    (exchange-NF-aux (rest nf)))))))

(defun exchange-NF-aux (nf) ;;cambiar literal en listos
  (if (null nf) 
    NIL
    (let ((lst (first nf)))
      (mapcan #'(lambda (x) 
		  (combine-elt-lst 
		    x 
		    (exchange-NF-aux (rest nf)))) 
	      (if (literal-p lst) (list lst) (rest lst))))))
#|
(print "exchange nf")
(print "(v a (^ b c) d)")
(print (exchange-NF '(v a (^ b c) d)))

(print "exchange nf aux")
(print "((^ b c) d)")
(print (exchange-NF-aux '((^ b c) d)))
(print "((^ b c) d e)")
(print (exchange-NF-aux '((^ b c) d e)))
|#

(defun simplify (connector lst-wffs ) ;; hace nada?
  (if (literal-p lst-wffs)
    lst-wffs                    
    (mapcan #'(lambda (x) 
		(cond 
		  ((literal-p x) (list x)) ;;cuando esta un literal, simplemente devuelve x
		  ((equal connector (first x))
		   (mapcan 
		     #'(lambda (y) (simplify connector (list y))) 
		     (rest x))) 
		  (t (list x))))               ;;cuando las connectores no estan los mismos, devuelve x
	    lst-wffs)))

#|
(print "simplify")
(print "v (a (^ b c))")
(print (simplify 'v '(a (^ b c) d)))
|#

(defun cnf (wff)
  (cond
    ((cnf-p wff) wff)
    ((literal-p wff)
     (list +and+ (list +or+ wff)))
    ((let ((connector (first wff))) 
       (cond
	 ((equal +and+ connector) 
	  (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))  ;;FBF maximo esta en FNC
	 ((equal +or+ connector) 
	  (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff))))))))))) ;;FBF maximo esta en FND, cambiar forma despues de simplificar

#|
(print "Ejemplos")
(print (cnf 'a))

(print (cnf '(v (¬ a) b c)))
(print (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (¬ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(print (exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s )))
(print (cnf '(^ (v a b (^ y r s) (v k l)) c (¬ d) (^ e f (v h i) (^ o p)))))
(print (cnf '(^ (v a b (^ y r s)) c (¬ d) (^ e f (v h i) (^ o p)))))
(print (cnf '(^ (^ y r s (^ p q (v c d))) (v a b))))
(print (cnf '(^ (v (¬ a) b c) (¬ e) r s 
		(v e f (¬ g) h) k (v m n) d)))
;;
(print (cnf '(^ (v p (¬ q)) (v k r (^ m  n)))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
(print (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) (^ (¬ p) m (v c d))) (v (¬ a) (¬ b))) g)))
;;
;; EJEMPLOS:
;;
(print (cnf NIL))              ; NIL
(print (cnf 'a))               ; (^ (V A))
(print (cnf '(¬ a)))           ; (^ (V (¬ A)))
(print (cnf '(V (¬ P) (¬ P)))) ; (^ (V (¬ P) (¬ P)))
(print (cnf '(V A)))           ; (^ (V A))
(print (cnf '(^ (v p (¬ q)) (v k r (^ m  n)))))
;;;   (^ (V P (¬ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (¬ B) D)  (V P Q E F R N (¬ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (¬ B) D)  (V P Q E F M N (¬ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
  (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) 
			 (^ (¬ p) m (v c d)))(v (¬ a) (¬ b))) g)))
;;;(^ (V (¬ Y)) (V R S (¬ P)) (V R S M) 
;;;   (V R S C D) (V R (¬ X) (¬ P)) 
;;;   (V R (¬ X) M) (V R (¬ X) C D)
;;;   (V (¬ A) (¬ B)) (V G))  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-connectors (cnf)
  ;;
  ;; 4.2.5 Completa el codigo
  ;;
  (if (literal-p cnf)
    cnf
    (mapcar 'eliminate-connectors (rest cnf))))

#|
(print (eliminate-connectors 'nil))
(print (eliminate-connectors (cnf '(^ (v p  (¬ q))  (v k  r  (^ m  n))))))
(print (eliminate-connectors
	 (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y)))))

(print (eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s ))))
(print (eliminate-connectors (cnf '(^ (v p  (¬ q)) (¬ a) (v k  r  (^ m  n))))))

(print (eliminate-connectors '(^)))
(print (eliminate-connectors '(^ (v p (¬ q)) (v) (v k r))))
(print (eliminate-connectors '(^ (v a b))))

;;   EJEMPLOS:
;;

(print (eliminate-connectors '(^ (v p (¬ q)) (v k r))))
;; ((P (¬ Q)) (K R))
(print (equal (eliminate-connectors '(^ (v p (¬ q)) (v k r))) '((P (¬ Q)) (K R))))
(print (eliminate-connectors '(^ (v p (¬ q)) (v q (¬ a)) (v s e f) (v b))))
(print (equal (eliminate-connectors '(^ (v p (¬ q)) (v q (¬ a)) (v s e f) (v b))) '((P (¬ Q)) (Q (¬ A)) (S E F) (B))))
;; ((P (¬ Q)) (Q (¬ A)) (S E F) (B))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
  ;;
  ;; 4.2.6 Completa el codigo
  ;;
  (eliminate-connectors (cnf (reduce-scope-of-negation (eliminate-conditional (eliminate-biconditional (infix-to-prefix wff)))))))

;;
;; EJEMPLOS:
;; 
#|
(print (wff-infix-to-cnf 'a))
(print (wff-infix-to-cnf '(¬ a)))
(print (wff-infix-to-cnf  '( (¬ p) v q v (¬ r) v (¬ s))))
(print (wff-infix-to-cnf  '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)))
;; ((P (¬ A) B) (P (¬ A) (¬ C)) (P (¬ A) D) ((¬ P) (¬ Q)) (Q P) (P) (E))
(print (equal (wff-infix-to-cnf  '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)) '((P (¬ A) B) (P (¬ A) (¬ C)) (P (¬ A) D) ((¬ P) (¬ Q)) (Q P) (P) (E))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-repeated-literals (k)
  ;;
  ;; 4.3.1 Completa el codigo
  ;;
  (cond 
    ((null k)
     nil)
    ((literal-p k)
     k)
    (t (let ((lit (first k)))
	 (cons lit (eliminate-repeated-literals (remove-if #'(lambda (x) (equal lit x)) (rest k))))))))

#|
;;
;; EJEMPLO:
;;
(print (eliminate-repeated-literals '(a b (¬ c) (¬ a) a c (¬ c) c a)))
(print (equal (eliminate-repeated-literals '(a b (¬ c) (¬ a) a c (¬ c) c a)) '(B (¬ A) (¬ C) C A)))
;;;   (B (¬ A) (¬ C) C A) ;;otro orden, pero semantico correcto
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-set-diff (elem1 elem2)
  (set-difference elem1 elem2 :test #'equal))

(defun cnf-equal-p (fnc1 fnc2)
  (and (equal (get-set-diff fnc1 fnc2) nil)
       (equal (get-set-diff fnc2 fnc1) nil)))

(defun reduced-clause-p (cnf)
  (reduce #'(lambda (prev elem) (and prev (literal-p elem))) cnf :initial-value t))
#|
(print "cnf equal")
(print (cnf-equal-p '(a b) '(a b)))
(print (cnf-equal-p '(a b) '(a)))
(print (cnf-equal-p '(a (b c) d) '((b c) a a d)))
(print (cnf-equal-p '(a b d) '((b c) a a d)))
|#

(defun eliminate-repeated-clauses (cnf) 
  ;;
  ;; 4.3.2 Completa el codigo
  ;;
  (cond
    ((literal-p cnf)
     cnf)
    ((reduced-clause-p cnf)
     cnf)
    (t (cons (first cnf) (eliminate-repeated-clauses (remove-if #'(lambda (elem) (cnf-equal-p (first cnf) elem)) (rest cnf)))))))

;;
;; EJEMPLO:
;;
#|
(print (eliminate-repeated-clauses '(a a b c a)))
(print (eliminate-repeated-clauses '((a b) (a b))))
(print (eliminate-repeated-clauses '(((¬ a) c) (c (¬ a)) ((¬ a) (¬ a) b c b) (a a b) (c (¬ a) b  b) (a b))))
;;; ((C (¬ A)) (C (¬ A) B) (A B))
(print '((C (¬ A)) (C (¬ A) B) (A B)))
(print (equal (eliminate-repeated-clauses '(((¬ a) c) (c (¬ a)) ((¬ a) (¬ a) b c b) (a a b) (c (¬ a) b  b) (a b))) '((C (¬ A)) (C (¬ A) B) (A B)))) ;;semantically equal
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume (K1 K2)
  ;;
  ;; 4.3.3 Completa el codigo
  ;;
  (if (equal nil (get-set-diff K1 K2))
    K1
    nil))
;;
;;  EJEMPLOS:
;;
#|
(print "subsume")
(print (subsume '(a) '(a b (¬ c))))
(print (equal (subsume '(a) '(a b (¬ c))) '((a))))
;; ((a))
(print (subsume NIL '(a b (¬ c))))
(print (equal (subsume NIL '(a b (¬ c))) '(NIL)))
;; (NIL)
(print (subsume '(a b (¬ c)) '(a) ))
(print (equal (subsume '(a b (¬ c)) '(a)) NIL))
;; NIL
(print (subsume '( b (¬ c)) '(a b (¬ c))))
(print (equal (subsume '( b (¬ c)) '(a b (¬ c))) '(b (¬ c))))
;; ( b (¬ c))
(print (subsume '(a b (¬ c)) '( b (¬ c))))
(print (equal (subsume '(a b (¬ c)) '( b (¬ c))) NIL))
;; NIL
(print (equal (subsume '(a b (¬ c)) '(d  b (¬ c))) nil))
;; nil
(print (equal (subsume '(a b (¬ c)) '((¬ a) b (¬ c) a)) '(A B (¬ C))))
;; (A B (¬ C))
(print (subsume '((¬ a) b (¬ c) a) '(a b (¬ c)) ))
(print (equal (subsume '((¬ a) b (¬ c) a) '(a b (¬ c))) nil))
;; nil
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : K (clausula), cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsumed-by-clause (k1 k2)
  (not (equal nil (subsume k2 k1))))

(defun subsumed-in-cnf-p (k cnf2)
  (reduce #'(lambda (prev elem) (or (subsumed-by-clause k elem) prev)) cnf2 :initial-value 'nil))

(defun eliminate-subsumed-clauses (cnf) 
  ;;
  ;; 4.3.4 Completa el codigo
  ;;
  (if (null cnf)
    nil
    (let* ((head (first cnf))
	   (tail (rest cnf))
	   (filtered-tail (remove-if #'(lambda (elem) (subsumed-by-clause elem head)) tail)))
      (if (subsumed-in-cnf-p head filtered-tail)
	(eliminate-subsumed-clauses filtered-tail)
	(cons head (eliminate-subsumed-clauses filtered-tail))))))

;;
;;  EJEMPLOS:
;;
#|
(print "eliminiate subsumed clauses")
(print (eliminate-subsumed-clauses 
	 '((a b c) (b c) (a (¬ c) b)  ((¬ a) b) (a b (¬ a)) (c b a))))
(print '((A (¬ C) B) ((¬ A) B) (B C))) ;; el orden no es importante
(print (eliminate-subsumed-clauses
	 '((a b c) (b c) (a (¬ c) b) (b)  ((¬ a) b) (a b (¬ a)) (c b a))))
(print '((B)))
(print (eliminate-subsumed-clauses
	 '((a b c) (b c) (a (¬ c) b) ((¬ a))  ((¬ a) b) (a b (¬ a)) (c b a))))
(print '((A (¬ C) B) ((¬ A)) (B C)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tautology-p (K) 
  ;;
  ;; 4.3.5 Completa el codigo
  ;;
  (if (null k)
    nil
    (let ((literal (first K)))
      (if (negative-literal-p literal)
	(let ((letter (second literal)))
	  (if (not (null (member letter (rest k) :test #'equal)))
	    t
	    (tautology-p (rest k)))))
      (if (not (null (member (list +not+ literal) (rest k) :test #'equal)))
	t
	(tautology-p (rest k))))))

;;
;;  EJEMPLOS:
;;
#|
(print (tautology-p '(A (¬ A))))
(print (equal (tautology-p '((¬ B) A C (¬ A) D)) 't)) ;;; T 
(print (equal (tautology-p '((¬ B) A C D)) nil))       ;;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
  ;;
  ;; 4.3.6 Completa el codigo
  ;;
  (remove-if #'tautology-p cnf))

;;
;;  EJEMPLOS:
;;
#|
(print (eliminate-tautologies 
	 '(((¬ b) a) (a (¬ a) b c) ( a (¬ b)) (s d (¬ s) (¬ s)) (a))))
;; (((¬ B) A) (A (¬ B)) (A))

(print (eliminate-tautologies '((a (¬ a) b c))))
;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf (cnf) 
  ;;
  ;; 4.3.7 Completa el codigo
  ;;
  (eliminate-subsumed-clauses (eliminate-tautologies (eliminate-repeated-clauses (mapcar #'eliminate-repeated-literals cnf)))))

;;
;;  EJEMPLOS:
;;
#|
(print (simplify-cnf '((a a) (b) (a) ((¬ b)) ((¬ b)) (a b c a)  (s s d) (b b c a b))))
(print '((B) ((¬ B)) (S D) (A))) ;; en cualquier orden
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ¬lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neutral-clause-p (_lambda clause)
  (and (null (member _lambda clause :test #'equal))
       (null (member (list +not+ _lambda) clause :test #'equal))))

(defun extract-neutral-clauses (_lambda cnf) 
  ;;
  ;; 4.4.1 Completa el codigo
  ;;
  (remove-if-not #'(lambda (clause) (neutral-clause-p _lambda clause)) cnf))


;;
;;  EJEMPLOS:
;;
#|
(print (extract-neutral-clauses 'p
				'((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s))))
;; ((R (¬ S) Q) ((¬ R) S))


(print (null (extract-neutral-clauses 'r NIL)))
;; NIL

(print (equal (extract-neutral-clauses 'r '(NIL)) '(NIL)))
;; (NIL)

(print (equal (extract-neutral-clauses 'r
				       '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s))) 
	      '((P Q) (A B P) (A (¬ P) C))))

(print (null (extract-neutral-clauses 'p
				      '((p (¬ q) r) (p q) (r (¬ s) p q) (a b p) (a (¬ p) c) ((¬ r) p s)))))
;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-has-positive-literal-p (_lambda clause)
  (not (null (member _lambda clause :test #'equal))))

(defun extract-positive-clauses (_lambda cnf) 
  ;;
  ;; 4.4.2 Completa el codigo
  ;;
  (remove-if-not #'(lambda (clause) (clause-has-positive-literal-p _lambda clause)) cnf))

;;
;;  EJEMPLOS:
;;
#|
(print (equal (extract-positive-clauses 'p
					'((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
	      '((P (¬ Q) R) (P Q) (A B P))))


(print (null (extract-positive-clauses 'r NIL)))
;; NIL
(print (null (extract-positive-clauses 'r '(NIL))))
;; NIL
(print (equal (extract-positive-clauses 'r
					'((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
	      '((P (¬ Q) R) (R (¬ S) Q))))
(print (null (extract-positive-clauses 'p
				       '(((¬ p) (¬ q) r) ((¬ p) q) (r (¬ s) (¬ p) q) (a b (¬ p)) ((¬ r) (¬ p) s)))))
;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ¬lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-has-negative-literal-p (_lambda clause)
  (not (null (member (list +not+ _lambda) clause :test #'equal))))

(defun extract-negative-clauses (_lambda cnf) 
  ;;
  ;; 4.4.3 Completa el codigo
  ;;
  (remove-if-not #'(lambda (clause) (clause-has-negative-literal-p _lambda clause)) cnf))

;;
;;  EJEMPLOS:
;;
#|
(print (equal (extract-negative-clauses 'p
					'((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
	      '((A (¬ P) C))))

(print (null (extract-negative-clauses 'r NIL)))
;; NIL
(print (null (extract-negative-clauses 'r '(NIL))))
;; NIL
(print (equal (extract-negative-clauses 'r
					'((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
	      '(((¬ R) S))))
(print (null (extract-negative-clauses 'p
				       '(( p (¬ q) r) ( p q) (r (¬ s) p q) (a b p) ((¬ r) p s)))))
;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun member-of-p (_lambda clause)
  (reduce #'(lambda (prev elem) (or (equal elem _lambda) prev)) clause :initial-value 'nil))

(defun has-positive-p (_lambda clause)
  (member-of-p _lambda clause))

(defun has-negative-p (_lambda clause)
  (member-of-p (list +not+ _lambda) clause))

(defun has-literal-p (_lambda clause)
  (or (has-negative-p _lambda clause) 
      (has-positive-p _lambda clause)))

(defun literal-of (_lambda elem)
  (or (equal (list +not+ _lambda) elem) (equal elem _lambda)))

(defun resolve-on (_lambda K1 K2) 
  ;;
  ;; 4.4.4 Completa el codigo
  ;;
  (if (or (and (has-negative-p _lambda k1) (has-positive-p _lambda k2)) (and (has-positive-p _lambda k1) (has-negative-p _lambda k2)))
    (remove-if #'(lambda (elem) (literal-of _lambda elem)) (union k1 k2 :test #'equal))))


;;
;;  EJEMPLOS:
;;
#|
(print "resolve on")
(print (null (resolve-on 'p '(a p) '(b p))))
(print (resolve-on 'p '(a p) '(b (¬ p))))
(print (resolve-on 'p '(a p) '(a (¬ p))))
(print (resolve-on 'p '((¬ a) p) '((¬ a) (¬ p))))
(print (resolve-on 'p '(a b (¬ c) p) '((¬ p) b a q r s)))
;; (((¬ C) B A Q R S))

(print (resolve-on 'p '(a b (¬ c) (¬ p)) '( p b a q r s)))
;; (((¬ C) B A Q R S))

(print (null (resolve-on 'p '(p) '((¬ p)))))
;; (NIL)


(print (null (resolve-on 'p NIL '(p b a q r s))))
;; NIL

(print (null (resolve-on 'p NIL NIL)))
;; NIL

(print (resolve-on 'p '(a b (¬ c) (¬ p)) '(p b a q r s)))
;; (((¬ C) B A Q R S))

(print (null (resolve-on 'p '(a b (¬ c)) '(p b a q r s))))
;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES-aux (_lambda clause clauses)
  (mapcar #'(lambda (clause2) (resolve-on _lambda clause clause2)) clauses))

(defun build-RES (_lambda cnf)
  ;;
  ;; 4.4.5 Completa el codigo
  ;;
  (let ((alpha-neutral (extract-neutral-clauses _lambda cnf))
	(alpha-positive (extract-positive-clauses _lambda cnf))
	(alpha-negative (extract-negative-clauses _lambda cnf)))
    (remove-duplicates (union alpha-neutral (mapcan #'(lambda (clause) (build-RES-aux _lambda clause alpha-negative)) alpha-positive)) :test #'cnf-equal-p)))

;;
;;  EJEMPLOS:
;;
#|
(print "build res")
(print (null (build-RES 'p NIL)))
;; NIL
(print (build-RES 'P '((A  (¬ P) B) (A P) (A B))));; ((A B))
(print (build-RES 'P '((B  (¬ P) A) (A P) (A B))));; ((B A))

(print (equal (build-RES 'p '(NIL)) '(nil)))
;; (NIL)

(print (equal (build-RES 'p '((p) ((¬ p)))) '(nil)))
;; (NIL)

(print (build-RES 'q '((p q) ((¬ p) q) (a b q) (p (¬ q)) ((¬ p) (¬ q)))))
(print '((P) ((¬ P) P) ((¬ P)) (B A P) (B A (¬ P))))

(print (build-RES 'p '((p q) (c q) (a b q) (p (¬ q)) (p (¬ q)))))
;; ((A B Q) (C Q))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun same-atom-p (literal-a literal-b)
  (if (negative-literal-p literal-a)
    (if (negative-literal-p literal-b)
      (equal (second literal-a) (second literal-b))
      (equal (second literal-a) literal-b))
    (if (negative-literal-p literal-b)
      (equal literal-a (second literal-b))
      (equal literal-a literal-b))))

#|
(print "same-atom")
(print (same-atom-p 'a 'a))
(print (same-atom-p 'a 'b))
(print (same-atom-p 'a '(¬ a)))
(print (same-atom-p 'a '(¬ b)))
(print (same-atom-p '(¬ a) '(¬ a)))
|#


(defun get-literals (cnf)
  (mapcar #'(lambda (elem) (if (negative-literal-p elem) (second elem) elem))
	  (reduce #'(lambda (prev elem) (union prev elem :test #'same-atom-p)) 
		  cnf 
		  :initial-value '() )))

#|
(print "get literals")
(print (get-literals
	 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) (p) (q) ((¬ r)) ((¬ p) (¬ q) r)))) 
|#

(defun res-sat-aux (literals cnf)
  (cond
    ((null cnf)
     t)
    ((equal cnf '(nil))
     nil)
    ((not (null (member nil cnf)))
     nil)
    ((null literals)
     nil)
    (t (res-sat-aux (rest literals) (simplify-cnf (build-RES (first literals) cnf))))))

(defun  RES-SAT-p (cnf) 
  ;;
  ;; 4.5 Completa el codigo
  ;;
  (res-sat-aux (get-literals cnf) cnf))

;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
#|
(print "res sat")
(print (RES-SAT-p nil))  ;;; T
(print (RES-SAT-p '((p) ((¬ q))))) ;;; T 
(print (RES-SAT-p
	 '((a b d) ((¬ p) q) ((¬ c) a b) ((¬ b) (¬ p) d) (c d (¬ a))))) ;;; T 
(print (RES-SAT-p
	 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) ((¬ q)) ((¬ p) (¬ q) r)))) ;;;T
;;
;; UNSAT Examples
;;
(print (null (RES-SAT-p '(nil))))         ;;; NIL
(print (null (RES-SAT-p '((S) nil))))     ;;; NIL 
(print (null (RES-SAT-p '((p) ((¬ p)))))) ;;; NIL
(print (null (RES-SAT-p
	       '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) (p) (q) ((¬ r)) ((¬ p) (¬ q) r))))) ;;; NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
  ;;
  ;; 4.6 Completa el codigo
  ;;
  (let ((base (wff-infix-to-cnf wff))
	(under-test (wff-infix-to-cnf (list +not+ w))))
    (not (RES-SAT-p (append base under-test)))))

;;
;;  EJEMPLOS:
;;
#|
(print "logical-consequence-RES-SAT-p")
(print (null (logical-consequence-RES-SAT-p NIL 'a))) ;;; NIL
(print (null (logical-consequence-RES-SAT-p NIL NIL))) ;;; NIL
(print (logical-consequence-RES-SAT-p '(q ^ (¬ q)) 'a)) ;;; T 
(print (logical-consequence-RES-SAT-p '(q ^ (¬ q)) '(¬ a))) ;;; T 

(print (logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) 'q))
;; T

(print (logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) '(¬ q)))
;; T

(print (logical-consequence-RES-SAT-p '((p => q) ^ p) 'q))
;; T

(print (null (logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))))
;; NIL

(print (logical-consequence-RES-SAT-p 
	 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	 '(¬ a)))
;; T

(print (logical-consequence-RES-SAT-p 
	 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	 'a))
;; T

(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       'a)))
;; NIL
(print (wff-infix-to-cnf '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q))))))
(print (wff-infix-to-cnf (list +not+ 'a)))
(print (append 
	 (wff-infix-to-cnf '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))))
	 (wff-infix-to-cnf (list +not+ 'a))))

(print (logical-consequence-RES-SAT-p 
	 '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	 '(¬ a)))
;; T

(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       'q)))
;; NIL

(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       '(¬ q))))
;; NIL

(print (null (logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))))      ;; NIL
(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       'a))) ;; NIL
(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       'q))) ;; NIL
(print (null (logical-consequence-RES-SAT-p 
	       '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
	       '(¬ q))))
|#
;; EJERCICIO 5 ;;

;5.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 bfs (end queue net)
;;;	 Algoritmo busqueda en anchura con busqueda en grafo
;;;
;;;   INPUT:        end:  nodo final
;;;                 queue: Lista que contiene la lista con el nodo inicial
;;;                 net: grafo
;;;
;;;   OUTPUT: Si termina con exito lista desde el inicial al objectivo, si no nil
;;;
(defun bfs (end queue net) ;;Se le pasan 3 argmentos, el final, la cola con el nodo inicial, y el grafo
  (if (null queue) nil ;Si la cola esta vacia no tendriamos donde empezar por tanto devuelve nil
    (let ((path (first queue)))  ;coje la primera lista de la cola (path)
      (let ((node (first path)))  ;coje el nodo inicial de path
        (if (eql node end) (reverse path) ;Comprueba si es el nodo final, si es devuelve la ruta del nodo inicial a el, por eso el reverse
          (bfs end (append  ;Si no, llamada recursiva, pasandole el final,
                            (rest queue)
                            (new-paths path node net));La union del resto de la cola (puede estar vacia) y la expansion de ese nodo
               net))))));Y el grafo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 new-paths (path node net)
;;;	 Explora el nodo
;;;
;;;   INPUT:        path: camino
;;;                 node: nodo a explorar
;;;                 net: grafo
;;;
;;;   OUTPUT: lista de nodos a explorar
;;;
(defun new-paths (path node net);le pasa la lista del nodo y de donde viene, el nodo, y el grafo
  (mapcar #'(lambda(n);(ver primero el ultimo) a los sucesores les aÃ±ade la ruta que es el nodo padre, el padre del padre, etc hasta el inicial
                   (cons n path))
          (rest (assoc node net))));Con assoc busca en el grafo al nodo y sus sucesores, y coger los sucesores
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;5.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 shortest-path (start end net)
;;;	 Busqueda de camino mas corto
;;;
;;;   INPUT:        start: nodo desde que se empieza
;;;                 end: nodo final
;;;                 net: grafo
;;;
;;;   OUTPUT: el camino o nil si no lo encuentra
;;;
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;5.6
;;(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;  0: (SHORTEST-PATH A F ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;    1: (BFS F ((A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;      2: (NEW-PATHS (A) A ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;      2: NEW-PATHS returned ((D A))
;;      2: (BFS F ((D A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;        3: (NEW-PATHS (D A) D ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;        3: NEW-PATHS returned ((F D A))
;;        3: (BFS F ((F D A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;;        3: BFS returned (A D F)
;;      2: BFS returned (A D F)
;;    1: BFS returned (A D F)
;;  0: SHORTEST-PATH returned (A D F)
;;(A D F)



;5.8
;Recursion infinita
;(shortest-path 'a 'c '((a b) (b a) (c a b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 bfs-improved (end queue visto net)
;;;	 Algoritmo busqueda en anchura con busqueda en grafo
;;;
;;;   INPUT:        end:  nodo final
;;;                 queue: Lista que contiene la lista con el nodo inicial
;;;                 visto: lista donde se guardan los nodos visitados
;;;                 net: grafo
;;;
;;;   OUTPUT: lista sin sublistas encadenadas con todos los atomos de lst
;;;
(defun bfs-improved (end queue visto net) ;;Se le pasan 3 argumentos, el final, la cola con el nodo inicial, y el grafo
  (if (null queue) nil ;Si la cola esta vacia no tendriamos donde empezar por tanto devuelve nil
    (let ((path (first queue)))  ;coge la primera lista de la cola (path)
      (let ((node (first path)))  ;coge el nodo inicial de path
        (if (eql node end) (reverse path) ;Comprueba si es el nodo final, si es devuelve la ruta del nodo inicial a el, por eso el reverse
          (when (null (member node visto)) (bfs-improved end (append  ;Si no, llamada recursiva, pasandole el final,
                (rest queue)
                (new-paths path node net)) (cons node visto);La union del resto de la cola (puede estar vacia) y la expansion de ese nodo
              net)))))));Y el grafo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 shortest-path-improved (end queue net)
;;;	 Busqueda de camino mas corto sin repeticiones
;;;
;;;   INPUT:        end: nodo final
;;;                 queue: Lista que contiene la lista con el nodo inicial
;;;                 net: grafo
;;;
;;;   OUTPUT: el camino o nil si no lo encuentra
;;;
(defun shortest-path-improved (end queue net)
  (bfs-improved end queue '() net))

;(shortest-path-improved 'a '((c)) '((a b) (b a) (c a b))) ;(C A)

;5.7
;(setf grafo '((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H) (F B H) (G C D E H) (H D E F G)))
;(shortest-path 'F 'C grafo)

;;(shortest-path 'F 'C grafo)
;;  0: (SHORTEST-PATH F C
;                    ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                     (F B H) (G C D E H) (H D E F G)))
;   1: (BFS C ((F))
;            ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H) (F B H)
;             (G C D E H) (H D E F G)))
;      2: (NEW-PATHS (F) F
;                    ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                     (F B H) (G C D E H) (H D E F G)))
;      2: NEW-PATHS returned ((B F) (H F))
;      2: (BFS C ((B F) (H F))
;               ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H) (F B H)
;                (G C D E H) (H D E F G)))
;         3: (NEW-PATHS (B F) B
;                       ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                        (F B H) (G C D E H) (H D E F G)))
;         3: NEW-PATHS returned ((A B F) (D B F) (E B F) (F B F))
;         3: (BFS C ((H F) (A B F) (D B F) (E B F) (F B F))
;                 ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                  (F B H) (G C D E H) (H D E F G)))
;           4: (NEW-PATHS (H F) H
;                         ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                          (E A B G H) (F B H) (G C D E H) (H D E F G)))
;           4: NEW-PATHS returned ((D H F) (E H F) (F H F) (G H F))
;           4: (BFS C
;                   ((A B F) (D B F) (E B F) (F B F) (D H F) (E H F) (F H F)
;                    (G H F))
;                   ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                    (F B H) (G C D E H) (H D E F G)))
;             5: (NEW-PATHS (A B F) A
;                           ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                            (E A B G H) (F B H) (G C D E H) (H D E F G)))
;             5: NEW-PATHS returned ((B A B F) (C A B F) (D A B F) (E A B F))
;             5: (BFS C
;                     ((D B F) (E B F) (F B F) (D H F) (E H F) (F H F) (G H F)
;                      (B A B F) (C A B F) (D A B F) (E A B F))
;                     ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                      (F B H) (G C D E H) (H D E F G)))
;               6: (NEW-PATHS (D B F) D
;                             ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                              (E A B G H) (F B H) (G C D E H) (H D E F G)))
;               6: NEW-PATHS returned ((A D B F) (B D B F) (G D B F) (H D B F))
;               6: (BFS C
;                       ((E B F) (F B F) (D H F) (E H F) (F H F) (G H F)
;                        (B A B F) (C A B F) (D A B F) (E A B F) (A D B F)
;                        (B D B F) (G D B F) (H D B F))
;                       ((C A G) (A B C D E) (B A D E F) (D A B G H) (E A B G H)
;                        (F B H) (G C D E H) (H D E F G)))
;                 7: (NEW-PATHS (E B F) E
;                               ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                 7: NEW-PATHS returned ((A E B F) (B E B F) (G E B F) (H E B F))
;                 7: (BFS C
;                         ((F B F) (D H F) (E H F) (F H F) (G H F) (B A B F)
;                          (C A B F) (D A B F) (E A B F) (A D B F) (B D B F)
;                          (G D B F) (H D B F) (A E B F) (B E B F) (G E B F)
;                          (H E B F))
;                         ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                          (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                   8: (NEW-PATHS (F B F) F
;                                 ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                  (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                   8: NEW-PATHS returned ((B F B F) (H F B F))
;                   8: (BFS C
;                           ((D H F) (E H F) (F H F) (G H F) (B A B F) (C A B F)
;                            (D A B F) (E A B F) (A D B F) (B D B F) (G D B F)
;                            (H D B F) (A E B F) (B E B F) (G E B F) (H E B F)
;                            (B F B F) (H F B F))
;                           ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                            (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                     9: (NEW-PATHS (D H F) D
;                                   ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                    (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                     9: NEW-PATHS returned
;                          ((A D H F) (B D H F) (G D H F) (H D H F))
;                     9: (BFS C
;                             ((E H F) (F H F) (G H F) (B A B F) (C A B F)
;                              (D A B F) (E A B F) (A D B F) (B D B F) (G D B F)
;                              (H D B F) (A E B F) (B E B F) (G E B F) (H E B F)
;                              (B F B F) (H F B F) (A D H F) (B D H F) (G D H F)
;                              (H D H F))
;                             ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                              (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                       10: (NEW-PATHS (E H F) E
;                                      ((C A G) (A B C D E) (B A D E F)
;                                       (D A B G H) (E A B G H) (F B H)
;                                       (G C D E H) (H D E F G)))
;                       10: NEW-PATHS returned
;                             ((A E H F) (B E H F) (G E H F) (H E H F))
;                       10: (BFS C
;                                ((F H F) (G H F) (B A B F) (C A B F) (D A B F)
;                                 (E A B F) (A D B F) (B D B F) (G D B F)
;                                 (H D B F) (A E B F) (B E B F) (G E B F)
;                                 (H E B F) (B F B F) (H F B F) (A D H F)
;                                 (B D H F) (G D H F) (H D H F) (A E H F)
;                                 (B E H F) (G E H F) (H E H F))
;                                ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                 (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                         11: (NEW-PATHS (F H F) F
;                                        ((C A G) (A B C D E) (B A D E F)
;                                         (D A B G H) (E A B G H) (F B H)
;                                         (G C D E H) (H D E F G)))
;                         11: NEW-PATHS returned ((B F H F) (H F H F))
;                         11: (BFS C
;                                  ((G H F) (B A B F) (C A B F) (D A B F)
;                                   (E A B F) (A D B F) (B D B F) (G D B F)
;                                   (H D B F) (A E B F) (B E B F) (G E B F)
;                                   (H E B F) (B F B F) (H F B F) (A D H F)
;                                   (B D H F) (G D H F) (H D H F) (A E H F)
;                                   (B E H F) (G E H F) (H E H F) (B F H F)
;                                   (H F H F))
;                                  ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                   (E A B G H) (F B H) (G C D E H) (H D E F G)))
;                           12: (NEW-PATHS (G H F) G
;                                          ((C A G) (A B C D E) (B A D E F)
;                                           (D A B G H) (E A B G H) (F B H)
;                                           (G C D E H) (H D E F G)))
;                           12: NEW-PATHS returned
;                                 ((C G H F) (D G H F) (E G H F) (H G H F))
;                           12: (BFS C
;                                    ((B A B F) (C A B F) (D A B F) (E A B F)
;                                     (A D B F) (B D B F) (G D B F) (H D B F)
;                                     (A E B F) (B E B F) (G E B F) (H E B F)
;                                     (B F B F) (H F B F) (A D H F) (B D H F)
;                                     (G D H F) (H D H F) (A E H F) (B E H F)
;                                     (G E H F) (H E H F) (B F H F) (H F H F)
;                                     (C G H F) (D G H F) (E G H F) (H G H F))
;                                    ((C A G) (A B C D E) (B A D E F) (D A B G H)
;                                     (E A B G H) (F B H) (G C D E H)
;                                     (H D E F G)))
;                             13: (NEW-PATHS (B A B F) B
;                                            ((C A G) (A B C D E) (B A D E F)
;                                             (D A B G H) (E A B G H) (F B H)
;                                             (G C D E H) (H D E F G)))
;                             13: NEW-PATHS returned
;                                   ((A B A B F) (D B A B F) (E B A B F)
;                                    (F B A B F))
;                             13: (BFS C
;                                      ((C A B F) (D A B F) (E A B F) (A D B F)
;                                       (B D B F) (G D B F) (H D B F) (A E B F)
;                                       (B E B F) (G E B F) (H E B F) (B F B F)
;                                       (H F B F) (A D H F) (B D H F) (G D H F)
;                                       (H D H F) (A E H F) (B E H F) (G E H F)
;                                       (H E H F) (B F H F) (H F H F) (C G H F)
;                                       (D G H F) (E G H F) (H G H F) (A B A B F)
;                                       (D B A B F) (E B A B F) (F B A B F))
;                                      ((C A G) (A B C D E) (B A D E F)
;                                       (D A B G H) (E A B G H) (F B H)
;                                       (G C D E H) (H D E F G)))
;                             13: BFS returned (F B A C)
;                           12: BFS returned (F B A C)
;                         11: BFS returned (F B A C)
;                       10: BFS returned (F B A C)
;                     9: BFS returned (F B A C)
;                   8: BFS returned (F B A C)
;                 7: BFS returned (F B A C)
;               6: BFS returned (F B A C)
;             5: BFS returned (F B A C)
;           4: BFS returned (F B A C)
;         3: BFS returned (F B A C)
;       2: BFS returned (F B A C)
;     1: BFS returned (F B A C)
;   0: SHORTEST-PATH returned (F B A C)
; (F B A C)
