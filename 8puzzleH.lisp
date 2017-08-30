;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 3
;;;; 8 puzzle State Space Representations
;;;*************************************************
;;; Notes : All the functions take the entire node as a parameter
;;;                 except the heuristic functions
;;;*************************************************

;;; PROBLEM/GOAL : Given an arrangement of 8 numbers on a 3x3 grid,
;;;                Move them around until they are in the correct arrangment
;;; RULES        : Numbers can be moved one at a time into the blank space
;;;                Numbers cannot move diagonally, only left, right, up, or down

;;; 2 | 8 | 3        Startstate
;;; 1 | 6 | 4        ∆ = blank space
;;; 7 | ∆ | 5
;;; Below is an equivalent representation of what's above
;;; (2 8 3 1 6 4 7 ∆ 5)

;;; '(3 6 5 4 ∆ 8 1 2 7)
(defvar *Easy8puzzleS* '(2 8 3 1 6 4 7 ∆ 5)) ;start state
(defvar *Hard8puzzleS* '(8 3 5 2 ∆ 6 7 1 4))
(defvar *Impossible8puzzleS* '(8 2 3 1 6 4 7 ∆ 5))
(defvar *8puzzleG* '(1 2 3 8 ∆ 4 7 6 5)) ;end state
;(defvar *8puzzleG* '(1 2 3 4 5 6 7 8 ∆)) ;end state
(defvar *8puzzle* '(8puzzleHeuristic moveup moveright movedown moveleft)) ;moves
(defvar *8puzzleMH* '(manhattanHeuristic moveup moveright movedown moveleft))
(defvar *8puzzleLC* '(linearConflictHeuristic moveup moveright movedown moveleft))

;;;*************************************************
;;; Name : 8puzzleHeuristic
;;; Description: Default Heuristic,
;;;              Compares nth element of node_heur to nth element of goalstate
;;; Parameters : state_heur - state being compared to the goal
;;;              depth_heur - depth of the state
;;; Return: Number of elements out of place
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun 8puzzleHeuristic (state_heur depth_heur)
       (let ((counter 0))
            (dotimes (currindex 9)
                     (if (not (equal (nth currindex state_heur) '∆))  ;if the current tile is not blank
                         (cond ((equal (nth currindex state_heur) (nth currindex *8puzzleG*))
                                        NIL) ;if a tile is in the correct place, do nothing
                               (T      (setq counter (+ counter 1))) ;else, increment counter
                         );end of cond
                     );end of if
            );end of dotimes
            (return-from 8puzzleHeuristic (+ depth_heur counter) )  ;return the counter
        );end of let
);end of 8puzzleHeuristic

;;;*************************************************
;;; Name : manhattanHeuristic
;;; Description: Manhattan Distance Heuristic
;;;              Finds sum of distances between each numbers current location and goal location
;;; Parameters : state_manh - state being compared to goal
;;;              depth_manh - depth of the state
;;; Return: Manhattan Number
;;;*************************************************
;;; Notes : relies on findNumber function
;;;*************************************************

(defun manhattanHeuristic (state_manh depth_manh)
         ;requires searching through the goal state for the correct location
       (let ((Total 0)(Xdist NIL) (Ydist NIL) (goalIndex NIL))  ;goalIndex is the index of the current tile being evaluated, within the goal state
            (dotimes (index (length state_manh))  ;for each number/tile
                     (cond ((not (equal (nth index state_manh) '∆))  ;if not looking at the blank tile
                         (setq goalIndex (findNumber (nth index state_manh) *8puzzleG*))
                        ;;For X values. mod 3 index, subtract to find X distance
                         (setq Xdist (abs (- (mod index 3) (mod goalIndex 3))))
                        ;;For Y values. check ranges (0-2) y=0 (3-5) y=1 (6-8) y=2
                        ;;    subtract both indicies, check what range it falls into, for the value
                        ;;UPON INSPECTION THIS IS ACTUALLY FAULTY ;; example. tile in position 4, needs to be in position 3. 4 - 3 = 1, results in a ydist of 0, when ydist is actually 1
                         (cond ((and (>= (abs (- index goalIndex)) 0) (<= (abs (- index goalIndex)) 2))
                                     (setq Ydist 0))
                               ((and (>= (abs (- index goalIndex)) 3) (<= (abs (- index goalIndex)) 5))
                                     (setq Ydist 1))
                               ((and (>= (abs (- index goalIndex)) 6) (<= (abs (- index goalIndex)) 8))
                                     (setq Ydist 2))
                         );end of cond
                         (setq Total (+ Xdist Ydist Total))
                     ));end of cond - not the blank tile
            );end of dotimes
            (return-from manhattanHeuristic (+ depth_manh Total))  ;return summation of [(StateX - GoalX) + (StateY - GoalY)] for all tiles
       );end of let
);end of manhattanHeuristic

;;;*************************************************
;;; Name : linearConflictHeuristic
;;; Description: If tile x, y, and y's goal g are in the same line, and
;;;              if x is in the way of y from to g
;;;              Add 2 to the number of moves for y to get to g
;;; Parameters : state_line - state being compared to goal
;;;              depth_line - depth of state
;;; Return: Linear Conflict Number + Manhattan Distance Number
;;;*************************************************
;;; Notes : This is an extention of the manhattan distance heuristic.
;;;         I found the idea online
;;;*************************************************

(defun linearConflictHeuristic (state_line depth_line)
    ;;;(format t "~S~%~S~%" state_line *8puzzleG*)
      (let ((Total 0) (Xdist NIL) (Ydist NIL) (goalIndex))
          (dotimes (index (length state_line))  ;For each tile
                   (cond ((not (equal (nth index state_line) '∆)) ;if current tile not blank
                         (setq goalIndex (findNumber (nth index state_line) *8puzzleG*))  ;Find index of current tile in the goal state
                         (setq Xdist (abs (- (mod index 3) (mod goalIndex 3))))  ;Find Xdist
                        ;;Find Ydist
                         (cond ((and (>= (abs (- index goalIndex)) 0) (<= (abs (- index goalIndex)) 2))
                                     (setq Ydist 0))
                               ((and (>= (abs (- index goalIndex)) 3) (<= (abs (- index goalIndex)) 5))
                                     (setq Ydist 1))
                               ((and (>= (abs (- index goalIndex)) 6) (<= (abs (- index goalIndex)) 8))
                                     (setq Ydist 2))
                         );end of cond
       ;;;(format t "Index: ~D X: ~D Y: ~D~%" index Xdist Ydist)
                         (cond ((and (= Xdist 0) (= Ydist 2))  ;tiles are vertically in line
                               ;;if Ydist is 2 and interfering tile not a blank ∆
                               ;;i.e. if index is (0 6) (1 7) (2 8), check 3,4,5 respectively for ∆
                                     (cond ((or (= index 0) (= index 6))
                                                (if (equal (nth 3 state_line) '∆)
                                                     NIL
                                                     (setq Total (+ Total 2))
                                                ))
                                           ((or (= index 1) (= index 7))
                                                (if (equal (nth 4 state_line) '∆)
                                                     NIL
                                                     (setq Total (+ Total 2))
                                                ))
                                           ((or (= index 2) (= index 8))
                                                (if (equal (nth 5 state_line) '∆)
                                                     NIL
                                                     (setq Total (+ Total 2))
                                                ))
                                     ));end of Ydist cond
                               ((and (= Ydist 0) (= Xdist 2))  ;tiles are horizontally in line
                                  ;;if Xdist is 2 and the interfering tile is not blank ∆
                                  ;;i.e if index is (0 2) (3 5) (6 8) check 1 4 7 respectively for ∆
                                    (cond ((or (= index 0) (= index 2))
                                               (if (equal (nth 1 state_line) '∆)
                                                    NIL
                                                    (setq Total (+ Total 2))
                                               ))
                                          ((or (= index 3) (= index 5))
                                               (if (equal (nth 4 state_line) '∆)
                                                    NIL
                                                    (setq Total (+ Total 2))
                                               ))
                                          ((or (= index 6) (= index 8))
                                               (if (equal (nth 7 state_line) '∆)
                                                    NIL
                                                    (setq Total (+ Total 2))
                                               ))
                                    ));end of Xdist cond
                           ));end of cond
                      );end of cond not a blank tile
  ;;;(format t "Total: ~D~%" Total)
          );end of dotimes
          (return-from linearConflictHeuristic (+ Total (manhattanHeuristic state_line depth_line)))
      );end of let

       ;x must NOT be a corner tile. y must not be the center tile
       ;x must be adjacent
       ;Add this total to the heuristic obtained from Manhattan Distance Heuristic
);end of linearConflictHeuristic

;;;*************************************************
;;; Name : findNumber
;;; Description: compares the given number to each element in state to find nums index in state
;;; Parameters : num - number that elements of state are compared to
;;;              state - the goalstate, being iterated through
;;; Return: Index where num = state
;;;*************************************************
;;; Notes : Used in the manhattanHeuristic function
;;;*************************************************

(defun findNumber (num state)
       (dotimes (index (length state))
                (cond ((equal num (nth index state))
                              (return-from findNumber index)))
       )
);end of findNumber

;;;*************************************************
;;; Name : findBlank
;;; Description: Finds index that the blank tile is located at by comparing each element to the blank symbol
;;; Parameters : node_b - node with the state being evaluated
;;; Return : Returns index
;;;*************************************************
;;; Notes : lists start at index 0
;;;*************************************************

(defun findBlank (node_b)
       (let ((puzzle (getState node_b)) (curr (first (getState node_b)))(x 0))
            (loop while (not (equal curr '∆)) do    ;while the blank has not been found
                        (setq x (+ x 1))            ;increment counter
                        (setq puzzle (rest puzzle)) ;get ready for next iteration
                        (setq curr (first puzzle))
            );end of loop
            (return-from findBlank x) ;return the index that the blank tile is located at
       );end of let
);end of findBlank

;;;*************************************************
;;; Name : moveup
;;; Description: Moves a tile up into the blank spot
;;;              Swaps the blank tile with the tile below it
;;; Parameters : node_u - node with state being evaluated
;;;              heur_u - heuristic being used
;;; Return : New node
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun moveup (node_u heur_u)
       (let ( (puzzle (getState (copy-tree node_u))) (index (findBlank node_u)) )
            (cond ((< index 6)                                        ;if there's a tile below it (index < 6)
                      ;;swap blank space and tile (+3 blank) (-3 tile)
                      (setf (nth index puzzle) (nth (+ index 3) puzzle))  ;set the blank spot to the tile that's below it
                      (setf (nth (+ index 3) puzzle) '∆)                  ;set the tile that just moved up, to be blank
                      (return-from moveup (createNode puzzle node_u heur_u)))    ;return the new node
                  (T  (return-from moveup NIL)))                          ;else return NIL
      );end of let
);end of moveup

;;;*************************************************
;;; Name : movedown
;;;*************************************************
;;; Notes : same as moveup, just opposite
;;;*************************************************

(defun movedown (node_d heur_d)
       (let ( (puzzle (getState (copy-tree node_d))) (index (findBlank node_d)) )
            (cond ((> index 2)                                            ;if there's a tile above it (index > 3)
                      ;;swap blank space and tile (-3 blank) (+3 tile)
                      (setf (nth index puzzle) (nth (- index 3) puzzle))  ;set the blank spot to the tile that's above it
                      (setf (nth (- index 3) puzzle) '∆)                  ;set the tile that just moved down, to be blank
                      (return-from movedown (createNode puzzle node_d heur_d)))  ;return the new node
                  (T  (return-from movedown NIL)))                        ;else return NIL
       );end of let
);end of movedown

;;;*************************************************
;;; Name : moveleft
;;;*************************************************
;;; Notes : same as moveup, uses mod 3 for the condition
;;;*************************************************

(defun moveleft (node_l heur_l)
       (let ( (puzzle (getState (copy-tree node_l))) (index (findBlank node_l)) )
            (cond ((> (mod index 3) 0)                                    ;if there's a tile left of it (index mod 3 > 0)
                      ;;swap blank space and tile (-1 blank) (+1 tile)
                      (setf (nth index puzzle) (nth (- index 1) puzzle))  ;set the blank spot to the tile that's left of it
                      (setf (nth (- index 1) puzzle) '∆)                  ;set the tile that just moved left, to be blank
                      (return-from moveleft (createNode puzzle node_l heur_l)))  ;return the new node
                  (T  (return-from moveleft NIL)))                        ;else return NIL
       );end of let
);end of moveleft

;;;*************************************************
;;; Name : moveright
;;;*************************************************
;;; Notes : same as moveup, uses mod 3 for the condition
;;;*************************************************

(defun moveright (node_r heur_r)
       (let ( (puzzle (getState (copy-tree node_r))) (index (findBlank node_r)) )
            (cond ((< (mod index 3) 2)                                    ;if there's a tile right of it (index mod 3 < 2)
                      ;;swap blank space and tile (+1 blank) (-1 tile)
                      (setf (nth index puzzle) (nth (+ index 1) puzzle))  ;set the blank spot to the tile that's right of it
                (setf (nth (+ index 1) puzzle) '∆)                        ;set the tile that just moved right, to be blank
                      (return-from moveright (createNode puzzle node_r heur_r))) ;return the new node
                  (T  (return-from moveright NIL)))                       ;else return NIL
       );end of let
);end of moveright
