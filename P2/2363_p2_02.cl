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

(
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


