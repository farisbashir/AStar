;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 2
;;;; Depth & Breadth First Search
;;;*************************************************
;;; Notes : The term node is used to refer to the 3-tuple list
;;;         that is defined in the homework instructions
;;;         NIL values as a result of an invalid/illegal move are still added to the open list
;;;*************************************************

(defvar *openlist* nil)      ; will contain the nodes that have yet to be evaluated
(defvar *closedlist* nil)    ; will contain the nodes already evaluated
(defvar *solutionpath* nil)  ; will contain the path taken from the start state to the goal state

;;;*************************************************
;;; Name : resetLists
;;; Description: sets the global variables to null
;;; Parameters : N/A
;;;*************************************************
;;; Notes : used at the end of DFS and BFS
;;;*************************************************

(defun resetLists ()
       (setq *openlist* nil)
       (setq *closedlist* nil)
       (setq *solutionpath* ())
);end of resetLists

;;;*************************************************
;;; Name : DFS
;;; Description: Adds start to the openlist and calls DFSRecursion
;;; Parameters : start - 1st state of the game evaluated
;;;              goal - end state being searched for
;;;              moves - functions, i.e. choices that can be made which create the next nodes to be searched
;;;*************************************************
;;; Notes: A wrapper method, Calls the recursive method,
;;;*************************************************

(defun DFS (startstate goalstate moveOptions)
           (let ((startnode (list startstate NIL 0)))
           (addFrontOfOpenList startnode) ; Add start to openlist
           (setq *solutionpath* (cons startnode *solutionpath*)) ; Add start to be the first value in the solution path
           (DFSRecursion startnode goalstate moveOptions)) ; Call the recursive function
           (let ((solpath nil) (goalnode (first *closedlist*)))
                (setq solpath (findSolutionPath goalnode))
                (printListLengths)              ; print the length of the closed and open list
                (printSolutionPath solpath)           ; print out the solution path
                (resetLists)
              (return-from DFS solpath)
           );end of let
)


;;;*************************************************
;;; Name : DFSRecursion
;;; Description: It checks if the passed in node is the goal state
;;;              If it's not, it calls all the possible moves and
;;;              adds the new nodes to the front of the open list
;;;              It then recurses on the first node in the open list
;;; Parameters : node - current node being evaluated
;;;              goal - end node being searched for
;;;              moves - functions, i.e. choices that can be made which create the next nodes to be searched
;;;*************************************************
;;; Notes:
;;;*************************************************
(defun DFSRecursion (node goal moves)
          (if (not (null node))
              (format t "Front of Open List: ~A~%" node);
          )
          (cond ((and (null *openlist*) (null (first node)))
                        NIL)
                ((or (equal (first node) NIL) (equal 20 (getDepth node)))  ;if the node is NIL, move on to the next node in the openlist
                        (setq *openlist* (rest *openlist*))          ;remove it from the open list
                        (DFSRecursion (first *openlist*) goal moves)) ;recurse on the next node
                ((equal (first node) goal)              ; if node is equal to goal
                        (addClosedList node))            ; add it to the closed list
                (T (setq *openlist* (rest *openlist*))  ; else remove it from the openlist
                   (useMovesDFS moves node)             ; run every single possible move choice
                   (addClosedList node)                 ; add it to closed list
                   (DFSRecursion (first *openlist*) goal moves)) ; recurse on the next node
            );end of cond
);end of DFSRecursion

;;;*************************************************
;;; Name : useMovesDFS
;;; Description: Calls the functions specified to create new nodes, then adds them to the front of openlist
;;; Parameters : moves - the list of functions to go through and create new nodes with
;;;              node  - The node that the function will be used on
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun useMovesDFS (moves_u node_u)
       (let ((m moves_u) (m1 NIL))                        ; m = list of possible moves, m
       (dotimes (n (length moves_u))                      ; run loop for the number of possible moves
                (setq m1 (first m))                       ; m1 = the next move in the list of moves
                (addFrontOfOpenList (funcall m1 node_u))  ; run the move which creates a new node, and add it to the front of the openlist
                (setq m (rest m))                         ; prepare for the next iteration by removing the move that was just performed
       ));end of dotimes and let

);end of useMovesDFS

;;;*************************************************
;;; Name : BFS
;;; Description: Adds start to the openlist and calls DFSRecursion
;;; Parameters : start - 1st state of the game evaluated
;;;              goal - end state being searched for
;;;              moves - functions, i.e. choices that can be made which create the next nodes to be searched
;;;*************************************************
;;; Notes: A wrapper method, Calls the recursive method,
;;;*************************************************

(defun BFS (startstate goalstate moveOptions)
           (let ((startnode (list startstate NIL 0)))
           (addEndOfOpenList startnode) ; Add start to openlist
           (setq *solutionpath* (cons startnode *solutionpath*)) ; Add start to be the first value in the solution path
           (BFSRecursion startnode goalstate moveOptions)) ; Call the recursive function
           (let ((solpath nil) (goalnode (first *closedlist*)))
                (setq solpath (findSolutionPath goalnode))
                (printListLengths)              ; print the length of the closed and open list
                (printSolutionPath solpath)           ; print out the solution path
                (resetLists)
                (return-from BFS solpath)
           );end of let
)

;;;*************************************************
;;; Name : BFSRecursion
;;; Description: It checks if the passed in node is the goal state
;;;              If it's not, it calls all the possible moves and
;;;              adds the new nodes to the end of the open list
;;;              It then recurses on the first node in the open list
;;; Parameters : node - current node being evaluated
;;;              goal - end node being searched for
;;;              moves - functions, i.e. choices that can be made which create the next nodes to be searched
;;;*************************************************
;;; Notes:
;;;*************************************************
(defun BFSRecursion (node goal moves)
          (if (not (null node))
              (format t "Front of Open List: ~A~%" node);
          )
          (cond ((null *openlist*)
                        NIL)
                ((equal (first node) NIL)               ;if the node is NIL, move on to the next node in the openlist
                        (setq *openlist* (rest *openlist*))          ;remove it from the open list
                        (BFSRecursion (first *openlist*) goal moves)) ;recurse on the next node
                ((equal (first node) goal)              ; if node is equal to goal
                        (addClosedList node))           ; add it to the closed list
                (T (setq *openlist* (rest *openlist*))  ; else remove it from the openlist
                   (useMovesBFS moves node)             ; run every single possible move choice
                   (addClosedList node)                 ; add it to closed list
                   (BFSRecursion (first *openlist*) goal moves)) ; recurse on the next node
            );end of cond
);end of BFSRecursion

;;;*************************************************
;;; Name : useMovesBFS
;;; Description: Calls the functions specified to create new nodes, then adds them to the end of openlist
;;; Parameters : moves_u - the list of functions to go through and create new nodes with
;;;              node_u  - The node that the function will be used on
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun useMovesBFS (moves_u node_u)
       (let ((m moves_u) (m1 NIL))                        ; m = list of possible moves, m
       (dotimes (n (length moves_u))                      ; run loop for the number of possible moves
                (setq m1 (first m))                       ; m1 = the next move in the list of moves
                (addEndOfOpenList (funcall m1 node_u))  ; run the move which creates a new node, and add it to the end of the openlist
                (setq m (rest m))                         ; prepare for the next iteration by removing the move that was just performed
       ));end of dotimes and let

);end of useMovesBFS

;;;*************************************************
;;; Name : addEndOfOpenList
;;; Description: Adds the given node to the end of the open list
;;; Parameters : node_addEnd - the node to be added
;;;*************************************************
;;; Notes : Should be used in conjuction with BFS
;;;*************************************************

(defun addEndOfOpenList (node_addEnd)
       (setq *openlist* (reverse (cons node_addEnd (reverse *openlist*))))
);end of addEndOfOpenList

;;;*************************************************
;;; Name : addFrontOfOpenList
;;; Description: Adds the given node to the front of the open list
;;; Parameters : node_addFront - the node to be added
;;;*************************************************
;;; Notes : Should be used with DFS
;;;*************************************************

(defun addFrontOfOpenList (node_addFront)
       (cond ((null node_addFront)
                    NIL)
             (T (setq *openlist* (cons node_addFront *openlist*))))
);end of FrontOfOpenList

;;;*************************************************
;;; Name : addClosedList
;;; Description: Adds the given node to the closed list
;;; Parameters : node_Closed - the node being added
;;;*************************************************
;;; Notes : Adds it to the front of the closed list
;;;*************************************************

(defun addClosedList (node_Closed)
       (setq *closedlist* (cons node_Closed *closedlist*))
)

;;;*************************************************
;;; Name : getDepth
;;; Description: Returns the depth of the given node
;;; Parameters : node_depth - 3-tuple list which contains the depth as the 3rd element
;;;*************************************************
;;; Notes : It assumes the depth is always the 3rd element of the node
;;;*************************************************

(defun getDepth (node_depth)
       (let ((depth (first (reverse node_depth)))) ;assigns the depth of the given node to the variable "depth"
             (return-from getDepth depth))         ;returns the number signifying the depth of the node
);end of getDepth

;;;*************************************************
;;; Name : findSolutionPath
;;; Description: Starts from the goal state and works backwards to find the solution path
;;; Parameters : goalnode - 1st element should be equal to the goal state
;;;*************************************************
;;; Notes : It relies on the findParent method
;;;*************************************************

(defun findSolutionPath (goalnode)
       (let ((currnode goalnode) (solpath nil) (parent nil))
            (setq solpath (cons goalnode solpath))
            (loop while (> (getdepth currnode) 0) do      ;while the depth of the node is not 0
                        (setq parent (findParent currnode)) ;find the node's parent
                        (setq solpath (cons parent solpath)) ;add it to the solution path
                        (setq currnode parent)              ;set the current node to the parent
                  ;(format t "~S" currnode)
            );end of while loop
            (return-from findSolutionPath solpath)
       );end of let
);end of findSolutionPath

;;;*************************************************
;;; Name : findParent
;;; Description: Searches through the closed list until it finds the passsed in nodes's parent
;;; Parameters : node_find
;;;*************************************************
;;; Notes : Check nodes in the closed list whos depth is 1 less than node_find
;;;         Compares node_find 2nd element to a nodes 1st element
;;;         Assumes that most recent nodes on closed list have a higher depth
;;;*************************************************

(defun findParent (node_find)
       (let ((closedlist *closedlist*) (curr (first *closedlist*)))
       (loop
            (cond ((equal (- (getDepth node_find) 1) (getDepth curr))          ;if curr's depth is one less than node_find
                          (cond ((equal (first curr) (first (rest node_find))) ; and if the curr's state = the parent state of node_find
                                        (return-from findParent curr))         ;return curr as the parent
                                (T      (setq closedlist (rest closedlist))    ;otherwise prepare for the next iteration
                                        (setq curr (first closedlist)))))
                  (T      (setq closedlist (rest closedlist))
                          (setq curr (first closedlist)))
            );end of cond
      ));end of loop and let
);end of findParent

;;;*************************************************
;;; Name : printSolutionPath
;;; Description: Prints to the console every node from the start leading to the goal state
;;; Parameters : N/A
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun printSolutionPath (solutionpath)
       (format t "Solution Path: ")
       (let ((lp (length solutionpath)) (solpath solutionpath)) ;lp = length of solution path, solpath = *solutionpath*
       (dotimes (n lp)                        ; loop for the length of the solution path
                (let ((f (first (first solpath))))    ; f = first value in *solutionpath*
                (format t "~A " f)           ; print out f
                (setq solpath (rest solpath)) ; remove the the first element, to prepare for the next iteration
       )))
       (format t "~%")
);end of printSolutionPath

;;;*************************************************
;;; Name : printListLengths
;;; Description: Prints the number of elements in the open and closed lists
;;; Parameters : N/A
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun printListLengths ()
        (let ((lo (length *openlist*)) (lc (length *closedlist*)))
        (format t "Open List Length: ~D~%Closed List Length: ~D~%" lo lc)
        );end of let
);end of printListLengths
