(defun check-valid (v) 
  (and (not (equal nil v)) (not (every (lambda (elem) (= elem 0)) v))))

(write (check-valid '(1 2 3)))
(write-line "")
(write (check-valid '(0 2 3)))
(write-line "")
(write (check-valid '(0 0 0)))
(write-line "")
(write (check-valid nil))
(write-line "")

(defun array-mult (x y)
  (if (null x)
    0 
    (+ (* (first x) (first y)) (array-mult (rest x) (rest y)))))

(defun array_square (x) (array-mult x x))

(defun sc-rec (x y)
  (if (and (check-valid x) (check-valid y))
    (/ (array-mult x y) (* (sqrt (array_square x)) (sqrt(array_square y))))))

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

(defun map-mult (x y)
  (reduce '+ (mapcar '* x y)))

(defun map-square (x)
  (map-mult x x))

(defun sc-mapcar (x y)
  (if (and (check-valid x) (check-valid y))
    (/ (map-mult x y) (* (sqrt (map-square x)) (sqrt (map-square y))))))

(write-line "Sc map")
(write (sc-mapcar '(3 4) '(3 4)))
(write-line "")
(write (sc-mapcar  '(-1 -1) '(1 1)))
(write-line "")
(write (sc-mapcar '(0 1) '(1 0)))
(write-line "")

(defun get-sims (x vs)
  (mapcar (lambda (v-n) (sc-mapcar x v-n)) vs))

(write (get-sims '(1 2 3) '((1 2 3) (0 1 0) (4 5 6))))
(write-line "")

(write-line "Simularities")
(write (get-sims '(1 2 3) '((1 2 3) (0 1 0) (4 5 6))))
(write-line "")

(defun filter (x conf)
  (remove-if (lambda (elem) (< elem conf)) x))

(write (filter '(0 5 10 3 4) 4))
(write-line "")

(write-line "Filter")
(write (filter '(0 5 10 3 4) 4))
(write-line "")

(defun get-filtered-sims (x vs conf)
  (filter (get-sims x vs) conf))

(defun sc-conf (cat vs conf)
  (sort (get-filtered-sims cat vs conf) '<))

(write-line "Sc conf")
(write (sc-conf '(1 2 3) '((1 2 3) (0 1 0) (4 5 6)) 0.7))
(write-line "")

(defun get-sim-couples-with-func (text categories func)
  (mapcar (lambda (category) (cons (first category) (funcall func text (rest category)))) categories))

(write-line "Sim couples:")
(write (get-sim-couples-with-func '(1 2 3 4) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")

(defun get-max-sims (text categories func)
  (first (sort (get-sim-couples-with-func text categories func) (lambda (sim1 sim2) (> (cdr sim1) (cdr sim2))))))

(write-line "Max sim couples:")
(write (get-max-sims '(1 2 3 4) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")

(defun sc-classifier (cats texts func)
  (mapcar (lambda (text) (get-max-sims (rest text) cats func)) texts))

(write-line "get classifiers")
(write (sc-classifier '((0 1 2 3 4) (1 3 2 2 0) (2 3 5 8 0)) '((0 1 2 3 4) (1 3 2 1 0) (2 1 1 1 1)) 'sc-mapcar))
(write-line "")


(setf cats0 '((1 43 23 12) (2 33 54 24)))
(setf texts0 '((1 3 22 134) (2 43 26 58)))

(setf cats1 '((1 43)  (54 24)))
(setf texts1 '((1 31) (26 58)))

(setf cats2 '((1 43 23 12 44) (2 33 54 24 93) (3 35 33 22 12) (4 55 21 12 34) (5 90 22 48 9 8)))
(setf texts2 '((1 3 22 134 388) (2 43 26 58 57) (3 4 6 2 0) (4 56 33 00 11) (5 38 13 92 29)))

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

(write-line "Bisect")
(write (bisect #'(lambda (x) (sin (* 6.26 x))) 0.1 0.7 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.26 x))) 0.0 0.7 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.28 x))) 1.1 1.5 0.001))
(write-line "")
(write (bisect #'(lambda (x) (sin (* 6.28 x))) 1.1 2.1 0.001))
(write-line "")

(defun get-couples (lst)
  (mapcar 'cons lst (rest lst)))

(defun allroot (f lst tol)
  (mapcan #'(lambda (interval) (list (bisect f (car interval) (cdr interval) tol))) (get-couples lst)))

(write-line "All roots")
(write (allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001))
(write-line "")

(defun get-step-size (a b N) 
  (/ (- b a) (expt 2 N)))

(write-line "Step size")
(write (get-step-size 0.1 2.25 2))
(write-line "")

(defun create-base-list (a N)
  (make-list (+ 1 N) :initial-element a))

(defun number-sequence (start end)
  (loop for n from start to end collect n))

(write-line "Number sequence")
(write (number-sequence 0 4))
(write-line "")

(defun create-base-intervals (step-size no-steps)
(mapcar #'(lambda (x) (* x step-size)) (number-sequence 0 no-steps)))

(defun create-interval-list (a b N)
  (let ((step-size (get-step-size a b N))
	(no-steps (expt 2 N)))
    (mapcar '+ (create-base-list a no-steps) (create-base-intervals step-size no-steps))))

(write-line "Interval list")
(write (create-interval-list 0.1 2.25 2))
(write-line "")

(defun allind (f a b N tol) 
  (allroot f (create-interval-list a b N) tol))

(write-line "Allind")
(write (allind #'(lambda(x) (sin (* 6.28 x ))) 0.1 2.25 1 0.0001))
(write-line "")
(write (allind #'(lambda(x) (sin (* 6.28 x ))) 0.1 2.25 2 0.0001))
(write-line "")

(defun combine-elt-lst (-elt lst)
  (mapcar #'(lambda(elem) (append (if (listp -elt) -elt (list -elt)) (list elem))) lst))

(write-line "combine-elt-lst")
(write (combine-elt-lst 'a nil))
(write-line "")
(write (combine-elt-lst 'a '(1 2 3)))
(write-line "")
(write (combine-elt-lst '(a b c) '(1 2 3)))
(write-line "")

(defun combine-lst-lst (lst1 lst2)
  (mapcan #'(lambda (elem) (combine-elt-lst elem lst2)) lst1))

(write-line "combine-lst-lst")
(write (combine-lst-lst nil nil))
(write-line "")
(write (combine-lst-lst '(a b c) nil))
(write-line "")
(write (combine-lst-lst NIL '(a b c)))
(write-line "")
(write (combine-lst-lst '(a b c) '(1 2)))
(write-line "")

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
