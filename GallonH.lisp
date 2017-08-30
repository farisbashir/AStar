;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 3
;;;; Water Gallon State Space Representations
;;;*************************************************
;;; Notes : All the functions take the entire node as a parameter
;;;                 except for the heuristic functin
;;;         State: The first value is the 5 gallon jug, second is the 3 gallon jug
;;;*************************************************

;;; PROBLEM/GOAL : We have a 5 gallon jug and a 3 gallon jug,
;;;                and the the goal is to measure out exactly 4 gallons
;;; RULES :        We can refill the jugs or dump them out at any time
;;;                Water can be transfered between jugs, but we cannot estimate how much we pour out

(defvar *gallonS* '(0 0)) ;start state
(defvar *gallonG* '(4 0)) ;end state
(defvar *gallon* '(GallonHeuristic pour5to3 pour3to5 refill5 refill3 dump5 dump3)) ;moves

;;;*************************************************
;;; Name : GallonHeuristic
;;; Description: Default Heuristic
;;;              Specifically says a how many moves away a state is from the goal
;;; Parameters : state_heur - state being compared to goal
;;;              depth_heur - depth of the state
;;; Return: # Heuristic of given state
;;;*************************************************
;;; Notes : It is a brute force method that essentially tells the program exactly what to do
;;;         It's not a good heuristic
;;;*************************************************

(defun GallonHeuristic (state_heur depth_heur)
       ;; absolute value of (4 - # of gallons in the 5 gallon jug)
       (cond  ((or (equal state_heur '(4 3)) (equal state_heur '(4 0)))
                   (return-from GallonHeuristic depth_heur))
              ((or (equal state_heur '(5 2)) (equal state_heur '(1 3)))
                   (return-from GallonHeuristic (+ depth_heur 1)))
              ((or  (equal state_heur '(0 2)) (equal state_heur '(1 0)))
                    (return-from GallonHeuristic (+ depth_heur 2)))
              ((or  (equal state_heur '(2 0)) (equal state_heur '(5 1)))
                    (return-from GallonHeuristic (+ depth_heur 3)))
              ((or  (equal state_heur '(2 3)) (equal state_heur '(3 3)))
                   (return-from GallonHeuristic (+ depth_heur 4)))
              ((or  (equal state_heur '(5 0)) (equal state_heur '(3 0)))
                    (return-from GallonHeuristic (+ depth_heur 5)))
              (T 6))
);end of GallonHeuristic

;;;*************************************************
;;; Name : dump5
;;; Description: set the first value equal to 0
;;; Parameters : node_d5 - node with the state being evaluated
;;;              heur_ - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to emptying the 5 gallon jug
;;;*************************************************

(defun dump5 (node_d5 heur_d5)
      (let ((state (getState node_d5)))
           (cond ((equal 0 (first state)) ; if 1st element is 0
                        (return-from dump5 NIL))   ; return NIL
                 (T     (setq state (cons 0 (rest state))) ; else change it equal to 0
                        (return-from dump5 (createNode state node_d5 heur_d5)))) ;and return a new node
      )
)

;;;*************************************************
;;; Name : dump3
;;; Description: set the second value equal to 0
;;; Parameters : node_d3 - node with the state being evaluated
;;;              heur_d3 - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to emptying the 3 gallon jug
;;;*************************************************

(defun dump3 (node_d3 heur_d3)
       (let ((state (getState node_d3)))
            (cond ((equal 0 (first (reverse state))); if 2nd element is not 0
                          (return-from dump3 NIL))   ; return NIL
                  (T     (setq state (reverse (cons 0 (rest (reverse state)))))
                          (return-from dump3 (createNode state node_d3 heur_d3))); change it equal to 0 and return a new node
            );end of cond
       )
);end of dump3

;;;*************************************************
;;; Name : refill5
;;; Description: Sets the first value equal to 5
;;; Parameters : node_r5 - node with the state being evaluated
;;;              heur_r5 - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to refilling the 5 gallon jug
;;;*************************************************

(defun refill5 (node_r5 heur_r5)
       (let ((state (getState node_r5)))
          (cond ((equal 5 (first state))          ; if 1st element is 5
                        (return-from refill5 NIL)) ; return NIL
                (T      (setq state (cons 5 (rest state)))   ; else change it equal to 5 and return a new node
                        (return-from refill5 (createNode state node_r5 heur_r5))))
       )
);end of refill5

;;;*************************************************
;;; Name : refill3
;;; Description: Set the second value equal to 3
;;; Parameters : node_r3 - node with the state being evaluated
;;;              heur_r3 - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to refilling the 3 gallon jug
;;;*************************************************

(defun refill3 (node_r3 heur_r3)
       (let ((state (getState node_r3)))
          (cond ((equal 3 (first (reverse state))); if 2nd element is not 3
                        (return-from refill3 NIL));return NIL
                (T      (setq state (reverse (cons 3 (rest (reverse state)))))
                        (return-from refill3 (createNode state node_r3 heur_r3)))) ;change it equal to 3 and return a new node
        )
);end of refill3

;;;*************************************************
;;; Name : pour5to3
;;; Description: Subtract from the first value and add to the second value,
;;;              until the second value is equal to 3, or the first value equals 0
;;; Parameters : node_p5 - node with the state being evaluated
;;;              heur_p5 - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to pouring water from the 5 gallon to the 3 gallon jug
;;;*************************************************

(defun pour5to3 (node_p5 heur_p5)
       (let ((state (getState node_p5)))
            (let ((5gal (first state)) (3gal (first (reverse state))))
            (cond ((or (equal 0 5gal) (equal 3 3gal)) ;if 1st element equal 0 or 2nd element does equal 3
                       (return-from pour5to3 NIL)) ;return NIL
                  (T
                       (loop while (and (not (equal 0 5gal)) (not (equal 3 3gal))) do  ;while 1st element does not equal 0 and 2nd element does not equal 3
                                   (setq 5gal (- 5gal 1))    ;substract one from first element, add one to 2nd element
                                   (setq 3gal (+ 3gal 1))
                       )
                       (setq state (cons 5gal (rest state)))
                       (setq state (reverse (cons 3gal (rest (reverse state)))))
                       (return-from pour5to3 (createNode state node_p5 heur_p5)))) ;return new node
            )
       )
);end of pour5to3

;;;*************************************************
;;; Name : pour3to5
;;; Description: Subtract from the second value and add to the second value,
;;;              untill the first value is equal to 5 or the second value equals 0
;;; Parameters : node_p3 - node with the state being evaluated
;;;              heur_p3 - heuristic being used
;;; Return: New node
;;;*************************************************
;;; Notes : Equivalent to pouring water from the 3 gallon to the 5 gallon jug
;;;*************************************************

(defun pour3to5 (node_p3 heur_p3)
      (let ((state (getState node_p3)))
           (let ((5gal (first state)) (3gal (first (reverse state))))
           (cond ((or (equal 5 5gal) (equal 0 3gal)) ;if 1st element equal 0 or 2nd element does equal 3
                      (return-from pour3to5 NIL)) ;return NIL
                 (T
                      (loop while (and (not (equal 5 5gal)) (not (equal 0 3gal))) do  ;while 2nd element does not equal 0 and 1st element does not equal 5
                                  (setq 5gal (+ 5gal 1))    ;substract one from 2nd element, add one to 1st element
                                  (setq 3gal (- 3gal 1))
                      )
                      (setq state (cons 5gal (rest state)))
                      (setq state (reverse (cons 3gal (rest (reverse state)))))
                      (return-from pour3to5 (createNode state node_p3 heur_p3)))) ;return new node
           )
      )
)
