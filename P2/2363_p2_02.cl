(defparameter *planets* '(Mallory Avalon Katril Kentares Davion Proserpina Sirtis))
(defparameter *white-holes*
  '(
    (Mallory Katril 10) (Mallory Proserpina 15) 
    (Avalon Mallory 6.4) (Avalon Proserpina 8.6)
    (Katril Davion 9) (Katril Mallory 10) 
    (Kentares Avalon 3) (Kentares Katril 10) (Kentares Proserpina 7) 
    (Davion Sirtis 6) (Davion Proserpina 5) 
    (Proserpina Mallory 15) (Proserpina Davion 5) (Proserpina Sirtis 12) (Proserpina Avalon 8.6)
    (Sirtis Davion 6) (Sirtis Proserpina 12)))

(defparameter *worm-holes*
  '(
    (Mallory Katril 5) (Mallory Proserpina 11) (Mallory Avalon 9)
    (Avalon Mallory 9) (Avalon Kentares 4)
    (Katril Sirtis 10) (Katril Davion 5) (Katril Mallory 5)
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Davion Katril 5) (Davion Sirtis 8)
    (Proserpina Mallory 11) (Proserpina Sirtis 9) (Proserpina Kentares 12)
    (Sirtis Proserpina 9) (Sirtis Davion 8) (Sirtis Katril 10)))

(defparameter *sensors*
  '(
    (Mallory 12)
    (Avalon 15)
    (Katril 9)
    (Kentares 14)
    (Davion 5)
    (Proserpina 7)
    (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden* '(Avalon))
(defparameter *planets-mandatory* '(Katril Proserpina))
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states              	; List of states
  initial-state       	; Initial state
  f-goal-test		; reference to a function that determines whether 
  ; a state fulfills the goal 
  f-h                 	; reference to a function that evaluates to the 
  ; value of the heuristic of a state
  operators)    	; list of operators (references to functions) 
; to generate actions, which, in their turn, are 
; used to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in the search algorithm
;;
(defstruct node 
  state        	; state label
  parent       	; parent node
  action				; action that generated the current node from its parent
  (depth 0)    	; depth in the search tree
  (g 0)        	; cost of the path from the initial state to this node
  (h 0)					; value of the heuristic
  (f 0))       	; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name         ; Name of the operator that generated the action
  origin       ; State on which the action is applied
  final        ; State that results from the application of the action
  cost )       ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; Name of the search strategy
  node-compare-p)		; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; f-h-galaxy 
;; Returns the approximated distance from a planet to the goal
;; state: name of the planet
;; sensors: list of (k v) pairs for h
;; devuelves: approximacion h(x) for the distance between state and destination
;;;;;;;;;;;;;;;;;;;;;;;

(defun f-h-galaxy (state sensors)
  (second (assoc state sensors)))

#|
(print (equal (f-h-galaxy 'Sirtis *sensors*) 0))
(print (equal (f-h-galaxy 'Avalon *sensors*) 15))
(print (equal (f-h-galaxy 'Earth *sensors*) nil)) ;-> NIL
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigate-white-hole
;; returns all possible paths through white holes from the planet "state"
;; state: nombre del planeto
;; white-holes: list of white hole paths leading from states to other planets
;; returns: actions through white holes possible from the current state

(defun action-name-route (name route)
  (make-action :name name
	       :origin (first route)
	       :final (second route)
	       :cost (third route)))

(defun create-action-white-hole (route)
  (action-name-route "navigate-white-hole" route))

(defun create-action-worm-hole (route)
  (action-name-route "navigate-worm-hole" route))

(defun get-routes-for (state routes)
  (remove-if #'(lambda (route) (not (equal (first route) state))) routes))

(defun navigate-white-hole (state white-holes)
  (mapcar #'(lambda (route) (create-action-white-hole route)) (get-routes-for state white-holes)))

(defun filter-forbidden (routes forbidden)
  (remove-if #'(lambda (route) (find (second route) forbidden :test #'equal)) routes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigate-worm-hole
;; returns all possible paths through worm holes from the planet "state"
;; state: name of the planet
;; worm-holes: list of worm hole paths leading from states to other planets
;; forbidden: planets which may not be visited
;; returns: actions through worm holes possible from the current state

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (mapcar #'(lambda (route) (create-action-worm-hole route)) (filter-forbidden (get-routes-for state worm-holes) planets-forbidden)))

#|
(print (navigate-white-hole 'Kentares *white-holes*))
(print (navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*))
(print (navigate-worm-hole 'Mallory *worm-holes* nil))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f-goal-test-galaxy
;; goal test, checks whether current state is a destination and all mandatory planets have been visited
;; node: current location, with nodepath
;; planets-destination: planets which are a viable end state
;; planets-mandatory: planets which must be visited
;; returns: true or false

(defun visited (planets nodepath)
  (cond 
    ((null planets)
     t)
    ((null nodepath)
     nil)
    ((find (node-state nodepath) planets)
     (visited (remove-if #'(lambda (planet) (equal planet (node-state nodepath))) planets) (node-parent nodepath)))
    (t
      (visited planets (node-parent nodepath)))))

(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
  (and (find (node-state node) planets-destination) (visited planets-mandatory node)))

(defparameter node-01
  (make-node :state 'Avalon) )
(defparameter node-02
  (make-node :state 'Kentares :parent node-01))
(defparameter node-03
  (make-node :state 'Katril :parent node-02))
(defparameter node-04
  (make-node :state 'Kentares :parent node-03))

#|
(print (null (f-goal-test-galaxy node-01 '(Kentares Uranus) '(Avalon Katril))))
(print (null (f-goal-test-galaxy node-02 '(Kentares Uranus) '(Avalon Katril))))
(print (null (f-goal-test-galaxy node-03 '(Kentares Uranus) '(Avalon Katril))))
(print (f-goal-test-galaxy node-04 '(Kentares Uranus) '(Avalon Katril)))
|#

(defun navigate-white-hole-aux (state)
  (navigate-white-hole state *white-holes*))

(defun navigate-worm-hole-aux (state)
  (navigate-worm-hole state *worm-holes* *planets-forbidden*))

(defparameter *galaxy-M35*
  (make-problem
    :states 		*planets*
    :initial-state 	*planet-origin*
    :f-goal-test	#'(lambda (node)
			    (f-goal-test-galaxy node *planets-destination*
						*planets-mandatory*))
    :f-h		#'(lambda (node)
			    (second (assoc node *sensors*)))
    :operators	(list 'navigate-worm-hole-aux 'navigate-white-hole-aux)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EJERCICIO5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf node-00
      (make-node :state 'Proserpina :depth 12 :g 10 :f 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	(defun expand-node (node problem)
;;;	Expande el nodo en funcion del problema a tratar, devolviendo todos los nodos 
;;;   a los que se puede ir.
;;;
;;;	INPUT: 
;;;		node: Nodo a expandir
;;;		problem: Problema bajo estudio segun el cual realizar la expansion
;;;	OUTPUT:
;;;		Nodos a los que se puede ir desde el nodo expandido o NIL
;;;
(defun expand-node (node problem)
  (if (null node)
    NIL
    (if (null problem)
      NIL
      (mapcar #'(lambda (x) ( make-node 
			      :state (action-final x)
			      :parent node
			      :action x
			      ;;depth = 1+node-depth node
			      :depth (+ 1 (if (node-depth node) 
					    (node-depth node) 
					    0)) 
			      :g (+ (if (node-g node) 
				      (node-g node) 
				      0 ) 
				    (if(action-cost x) ;g=node-g + cost
				      (action-cost x) 
				      0 ))
			      :h (funcall (fn-name (problem-fn-h problem)) ;h=sensors
					  (action-final x) 
					  (fn-lst-args (problem-fn-h problem))) 
			      ;;f= g+h
			      :f (+ (+ (if (node-g node) 
					 (node-g node) 
					 0 ) 
				       (if(action-cost x) 
					 (action-cost x) 
					 0 )) 

				    (if (null (funcall (fn-name (problem-fn-h problem)) 
						       (action-final x) 
						       (fn-lst-args (problem-fn-h problem)))
					      ) 0 
				      (funcall (fn-name (problem-fn-h problem)) 
					       (action-final x) 
					       (fn-lst-args (problem-fn-h problem))) ))  

			      )) 
	      ;;todos los elementos de las 2 listas donde se encuentra el nombre de node-name

	      (append          

		(funcall(fn-name (first (problem-operators problem ))) 
		  (node-state node) 
		  (fn-lst-args (first (problem-operators problem )))) 

		(funcall (fn-name (second (problem-operators problem ))) 
			 (node-state node) 
			 (fn-lst-args (second (problem-operators problem )))))))))

;;;
;;;	EJEMPLOS:
;;;		(expand-node node-00 *galaxy-M35*)	;->Caso tipico	;->
;;;		(setf node-08 (make-node :state 'Tierra :depth 1 :g 1 :f 2))
;;;			(expand-node node-08 *galaxy-M35*)	;->NIL	;->Caso especial
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
    :name 'uniform-cost
    :node-compare-p #'node-g-<=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert a list of nodes into another list of nodes
;;;
;;; nodes: list of nodes to insert, unordered
;;; lst-nodes: list of nodes to insert 'nodes' into, ordered
;;; strategy: strategy to use

(defun insert-node-strategy (node lst-nodes strategy)
  (cond
    ((null lst-nodes)
     (list node))
     ((funcall (strategy-node-compare-p strategy) node (first lst-nodes))
      (cons node lst-nodes))
     (t 
       (cons (first lst-nodes) (insert-node-strategy node (rest lst-nodes) strategy)))))

#|
(print "insert node")
(print (insert-node-strategy (make-node :g 3) (list (make-node :g 4)) *uniform-cost*))
(print (insert-node-strategy (make-node :g 3) (list (make-node :g 1) (make-node :g 2) (make-node :g 4)) *uniform-cost*))
(print (insert-node-strategy (make-node :g 3) (list (make-node :g 1) (make-node :g 2) (make-node :g 2)) *uniform-cost*))
|#

(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (if (null nodes)
    lst-nodes
    (insert-nodes-strategy (rest nodes) (insert-node-strategy (first nodes) lst-nodes strategy) strategy)))

(defparameter node-01
  (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-02
  (make-node :state 'Kentares :depth 2 :g 50 :f 50) )

#|
(print (insert-nodes-strategy (list node-00 node-01 node-02)
			      lst-nodes-00
			      *uniform-cost*))

(print (insert-nodes-strategy (list node-00 node-01 node-02)
			      (sort (copy-list lst-nodes-00) #'<= :key #'node-g)
			      *uniform-cost*))

(print (insert-nodes-strategy '(4 8 6 2) '(1 3 5 7)
		       (make-strategy :name 'simple
				      :node-compare-p #'<)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ejercicio 7 
;; Definir estrategia para la búsqueda A*.
;; Inicializa una variable global cuyo valor sea la estrategia para realizar la búsqueda A*:
;;
;;;;;;;;;;;;;;;;;;;;


(defparameter *A-star*
  (make-strategy
    :name 'A-star
    :node-compare-p #'node-f-<=))

(defun node-f-<= (node-1 node-2)
  (<= (node-f node-1) (node-f node-2)))
