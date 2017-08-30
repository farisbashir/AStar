
(defvar *inputstart* NIL)
(defvar *inputgoal* NIL)

(defun setStart&Goal ()
       (format t "Enter a start state ~%")
       (setq *inputstart* (read))
       (format t "Enter a goal state ~%")
       (setq *inputgoal*  (read))
)

(defun solve8puzzle ()
       (setStart&Goal)
       (DFS *inputstart* *inputgoal* *8puzzle*)
       (format t "Type \"Go\" and press Enter to Proceed to Breadth First Search~%")
       (read)
       (BFS *inputstart* *inputgoal* *8puzzle*)

)
