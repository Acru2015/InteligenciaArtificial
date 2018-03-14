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

(defun get-actions (state operators)
  (mapcan #'(lambda (operator) (funcall operator state)) operators))

(defun generate-target-node (action parent f-h)
  (let* ((state (action-final action))
	 (depth (+ (node-depth parent) 1))
	 (g (+ (node-g parent) (action-cost action)))
	 (h (funcall f-h state)))
    (make-node :state state
	       :parent parent
	       :action action
	       :depth depth
	       :g g 
	       :h h
	       :f (+ g h))))

(defun generate-target-nodes (node f-h actions)
  (mapcar #'(lambda (action) (generate-target-node action node f-h)) actions))

(defun expand-node (node problem)
  (generate-target-nodes node (problem-f-h problem) (get-actions (node-state node) (problem-operators problem))))

#|
(print "Expand nodes")
(print (expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0) *galaxy-M35*))
(print (expand-node node-00 *galaxy-M35*))	;->Caso tipico	;->
(setf node-08 (make-node :state 'Tierra :depth 1 :g 1 :f 2))
(print (expand-node node-08 *galaxy-M35*))	;->NIL	;->Caso especial
|#

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

(defun node-f-<= (node-1 node-2)
  (<= (node-f node-1) (node-f node-2)))

(defparameter *A-star*
  (make-strategy
    :name 'A-star
    :node-compare-p #'node-f-<=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ejercicio 8
;; Busqueda
;; problem: problem to solve
;; strategy: the strategy to solve the search with
;; returns: the optimal path through the graph
;;
;;;;;;;;;;;

(defun fulfills-goal-test (problem node)
  (funcall (problem-f-goal-test problem) node))

(defun get-node-if-available (node node-list)
  (first (member node node-list)))

(defun expand-and-merge (node node-list problem strategy)
  (insert-nodes-strategy (expand-node node problem) node-list strategy))

(defun graph-search-aux (open-nodes closed-nodes problem strategy)
  (if (null open-nodes)
    nil
    (let ((current-node (first open-nodes)))
      (if (fulfills-goal-test problem current-node)
	current-node
	(let ((node-in-list (get-node-if-available current-node closed-nodes)))
	  (if (or (null node-in-list) (< (node-g current-node) (node-g node-in-list)))
	    (graph-search-aux (expand-and-merge current-node open-nodes problem strategy) (cons current-node closed-nodes) problem strategy)
	    (graph-search-aux (rest open-nodes) closed-nodes problem strategy)))))))

(defun graph-search (problem strategy)
  (graph-search-aux (list (make-node :state (problem-initial-state problem) :depth 0 :g 0)) '() problem strategy))


;;(print (graph-search *galaxy-M35* *A-star*))

(defun a-star-search (problem) 
  (graph-search problem *A-star*))

;;(print (a-star-search *galaxy-M35*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ejercicio 9
;; Solution-path
;; Function which shows the way take to the node from the start
;; node: endnode
;; returns: name of the states visited
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-of-property (getter node)
(if (null node)
    nil
    (append (path-of-property getter (node-parent node)) (list (funcall getter node)))))

(defun solution-path (node)
  (path-of-property #'node-state node))

;; (print "solution path")
;;(print (solution-path nil))
;;(print (solution-path (a-star-search *galaxy-M35*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action-sequence
;; Function which shows the sequence of actions taken to the node from the start
;; node: endnode
;; returns: actions executed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-sequence (node)
  (path-of-property #'node-action node))

;; (print "action sequence")
;;(print (action-sequence (a-star-search *galaxy-M35*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Exercise 10
;; Other strategies
;; depth first: depth first search strategy


(defun depth-first-node-compare-p (node-1 node-2)
  (> (node-depth node-1) (node-depth node-2)))

(defparameter *depth-first*
  (make-strategy
    :name 'depth-first
    :node-compare-p #'depth-first-node-compare-p))

;;(print "dfs")
;;(print (solution-path (graph-search *galaxy-M35* *depth-first*)))

;;
;; breadth first: depth first search strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun breadth-first-node-compare-p (node-1 node-2)
  (< (node-depth node-1) (node-depth node-2)))

(defparameter *breadth-first*
  (make-strategy
    :name 'breadth-first
    :node-compare-p #'breadth-first-node-compare-p))

;;(print "bfs")
;;(print (solution-path (graph-search *galaxy-M35* *breadth-first*)))
