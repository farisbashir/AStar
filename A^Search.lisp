;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 3
;;;; A * Search
;;;*************************************************
;;; Notes : The term node is used to refer to the 4-tuple list
;;;         (current-state parent-state depth heuristic)
;;;*************************************************

(defvar *open* NIL) ;Open List that new nodes are added to
(defvar *closed* NIL) ;Closed List that nodes already evaluated are added to

;;;*************************************************
;;; Name : A*Search
;;; Description: Creates the first node , adds it to the openlist and calls A*SearchRecursion
;;; Parameters : startstate - start condition of the "game"
;;;              goalstate - end configuration that is trying to be achieved
;;;              moveOptions - the list of possible moves that can be done to a given state
;;; Return: Solution path from the startstate to the goalstate
;;;*************************************************
;;; Notes : A wrapper method that calls the recursive function
;;;         The first element of moveOptions is the heuristic function being used
;;;*************************************************

(defun A*Search (startstate goalstate moveOptions)
       (let ((startnode (list startstate NIL 0 (funcall (first moveOptions) startstate 0))) (boolean NIL) (solpath NIL)) ;Create a new node HOW TO DEAL WITH 1st NIL HEURISTIC
            (setq *open* (cons startnode *open*))     ;add it to the openlist
            (setq boolean (A*SearchRecursion startnode goalstate moveOptions)) ;call Recurisve function

            (cond (boolean ;if it returns true
                        (format t "Open List Length: ~D~%" (length *open*))  ;print list lengths
                        (format t "Closed List Length: ~D~%" (length *closed*))
                        (setq solpath (findSolutionPath))
                        (printSolutionPath solpath)
                        (return-from A*Search solpath))  ;find and return solution path
                  (T (format t "Solution Path could not be found") NIL))  ;else return NIL
       );end of let
);end of A*Search

;;;*************************************************
;;; Name : A*SearchRecursion
;;; Description: Evaluates the given node, runs all possible moves on it to create the next nodes
;;;              Terminates when the goal state has been found, or when the openlist is NIL
;;; Parameters : currnode - node being evaluated
;;;              goal - end state it is trying to achieve
;;;              moves - list of possible moves that can be done on a given state
;;; Return: T if goalstate has been found
;;;*************************************************
;;; Notes : Relies on useMovesA* to create the new nodes
;;;         Has a depth limit of 20, i.e problem must be solvable within 20 moves
;;;*************************************************

(defun A*SearchRecursion (currnode goal moves)
       (format t "First Node on Open List: ~S~%" currnode)
       (cond ((and (equal currnode NIL) (equal *open* NIL)) ;if the openlist and currnode are NIL
                   (return-from A*SearchRecursion NIL))     ;return NIL
             ((equal goal (getState currnode))  ;else if currnode state = goal
                   (setq *closed* (cons currnode *closed*)) ;add currnode to the closed list
                   (return-from A*SearchRecursion T))  ;return T
             ((>= (getDepth currnode) 20)  ;else if the depth is greater than 20
                   (setq *open* (rest *open*)) ;remove currnode from openlist;
                   (setq *closed* (cons currnode *closed*))  ;add it to closelist ;
                   (return-from A*SearchRecursion (A*SearchRecursion (first *open*) goal moves)))  ;recurse
             (T    (setq *open* (rest *open*))  ;else remove currnode from openlist
                   (setq *closed* (cons currnode *closed*))  ;add it to closedlist
                   (useMovesA* currnode moves)  ;call the function useMovesA*
                   (return-from A*SearchRecursion (A*SearchRecursion (first *open*) goal moves)))  ;recurse
        );end of cond
);end of A*SearchRecursion

;;;*************************************************
;;; Name : useMovesA*
;;; Description: Runs through the moves on the given node to create new nodes, and adds them to the openlist
;;; Parameters : node_moves - node that moves functions are being called on
;;;              moveList - list of functions to be called
;;; Return: N/A
;;;*************************************************
;;; Notes : Relies on heuristicInsertSort to properly put new nodes in the openlist
;;;         Assumes that the moves functions will return a new node or NIL
;;;         First element of moveList must be the heuristic function
;;;*************************************************

(defun useMovesA* (node_moves moveList)
       (let ((currmove NIL) (movelistcopy moveList) (heuristic (first moveList)))
            (setq movelistcopy (rest movelistcopy))
            (dotimes (n (length movelistcopy))  ;do for the number of moves there are
            (setq currmove (first movelistcopy))  ;function call first element of moveList on node_moves
            (setq *open* (callInsertSort (funcall currmove node_moves heuristic) *open* *closed*))  ;insert new node into open list
            (setq movelistcopy (rest movelistcopy))  ;prepare for next iteration
        ));end of dotimes and let
);end of useMovesA*

;;;*************************************************
;;; Name : callInsertSort
;;; Description: checks if the node is NULL or if its state is already on the openlist
;;;              If so, it returns the passed in openlist, otherwise it calls heuristicInsertSort
;;; Parameters : node_call - newly created node trying to be added to openlist
;;;              openlist_call - contains the nodes yet to be evaluated
;;;              closedlist_calss - contains the nodes already evaluated
;;; Return: Openlist with properly inserted node
;;;*************************************************
;;; Notes : relies on memberOfOpenLists
;;;         A wrapper method for heuristicInsertSort
;;;*************************************************

(defun callInsertSort (node_call openlist_call closedlist_call)
       (cond ((or (null node_call) (memberOfLists node_call openlist_call closedlist_call)) ;if node_sort is null or already in backlist
                  (return-from callInsertSort openlist_call))  ;return openlist (nothing changes)
             (T (return-from callInsertSort (heuristicInsertSort node_call NIL openlist_call)))  ;else ,properly insert the node into the openlist
      );end of cond
);end of callInsertSort

;;;*************************************************
;;; Name : memberOfList
;;; Description: Checks if the passed in node's state can already be found in the open/closedlist
;;;              The purpose is to reduce duplicates
;;; Parameters : node_member - node whose state is being compared to states on open/closedlist
;;;              openlist - list of nodes that have yet to be evaluated in A*Search
;;;              closedlist - list of nodes already evaluated in A*Search
;;; Return: T if a duplicate can be found, NIL otherwise
;;;*************************************************
;;; Notes : used in callInsertSort
;;;*************************************************

(defun memberOfLists (node_member openlist closedlist)
       (let ((curr (first openlist)) (curr2 (first closedlist)))
            (dotimes (n (length openlist))  ;for every node in open/closedlist
                     (cond ((equal (getState node_member) (getState curr))  ;if given node's state = state of node on openlist
                                   (return-from memberOfLists T))  ; return true
                           ((equal (getState node_member) (getState curr2))  ;if given node's state = state of node on closedlist
                                         (return-from memberOfLists T))  ; return true
                           (T (setq openlist (rest openlist))  ;else, prepare for next iteration
                              (setq curr (first openlist))
                              (setq curr2 (first closedlist)))
                     );end of cond
            );end of dotimes
       );end of let
       (return-from memberOfLists NIL)  ;return NIL if duplicate state could not be found
);end of memberOfOpenlist

;;;*************************************************
;;; Name : heuristicInsertSort
;;; Description: Inserts node_sort into the given list, smaller heuristics before larger ones
;;; Parameters : node_sort - node to be inserted in the list
;;;              frontlist - portion of list already checked through
;;;              backlist - portion of list, yet to be checked
;;; Return: Original list with the node_sort properly inserted
;;;*************************************************
;;; Notes : frontlist will always initially be NIL, backlist will be *open*
;;;         Assumes the list is already sorted
;;;         Recursive Function
;;;*************************************************

(defun heuristicInsertSort (node_sort frontlist backlist)
        (cond ((NULL backlist)  ;if backlist is null
                     (setq backlist (cons node_sort backlist)) ;add node_sort to backlist
                     (return-from heuristicInsertSort (append frontlist backlist))) ;return appended frontlist and backlist
              ((<= (getHeuristic node_sort) (getHeuristic (first backlist)))  ;else if node_sort is less than or equal to heuristic of first node in backlist
                     (setq backlist (cons node_sort backlist))  ;add node_sort to the front of backlist
                     (setq backlist (append frontlist backlist))  ;append frontlist and backlist,
                     (return-from heuristicInsertSort backlist))  ;return the combined list
              (T  ;else if heuristic of node_sort is greater than heursitc of first node in backlist
                     (setq frontlist (reverse (cons (first backlist) (reverse frontlist)))) ;add first node of backlist to end of frontlist
                     (setq backlist (rest backlist)) ;remove first node in backlist
                     (return-from heuristicInsertSort (heuristicInsertSort node_sort frontlist backlist)))  ;return a recursive call
        );end of cond
);end of heuristicInsertSort

;;;*************************************************
;;; Name : findSolutionPath
;;; Description: Starts from the goal node and works backwards to find the solution path
;;;              Finds a path from the end state to the first node
;;; Parameters : N/A
;;; Return: Path from first node to goal
;;;*************************************************
;;; Notes : It relies on the findParent method
;;;*************************************************

(defun findSolutionPath ()
       (let ((currnode (first *closed*)) (solpath NIL) (parent NIL))  ;currnode = goalnode
            (setq solpath (cons currnode solpath))  ;add goalnode to solutionpath
            (loop while (> (getDepth currnode) 0) do  ;while depth is not 0
                        (setq parent (findParent currnode))  ;find node's parent
                        (setq solpath (cons parent solpath))  ;add parent to solution path
                        (setq currnode parent)  ;prepare for next iteration
            );end of while loop
            (return-from findSolutionPath solpath)
       );end of let
);end of findSolutionPath

;;;*************************************************
;;; Name : findParent
;;; Description: Searches through the closed list until it finds the passed in nodes's parent
;;; Parameters : node_find
;;; Return: Parent node of node_find
;;;*************************************************
;;; Notes : Check nodes in the closed list whos depth is 1 less than node_find
;;;         Compares node_find 2nd element to a nodes 1st element
;;;         Assumes that most recent nodes on closed list have a higher depth
;;;*************************************************

(defun findParent (node_find)
       (let ((closedlist *closed*) (curr (first *closed*)))  ;curr = first value in the closed list
       (loop  ;terminates when the parent node is found
            (cond ((equal (- (getDepth node_find) 1) (getDepth curr))          ;if curr's depth is one less than node_find
                          (cond ((equal (getState curr) (getParent node_find)) ; and if the curr's state = the parent state of node_find
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
;;; Description: prints out, in order, the states of each node in the solution path
;;; Parameters : solutionpath - list with the nodes to be printed
;;; Return: N/A
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun printSolutionPath (solutionpath)
       (let ((currnode NIL))
           (dotimes (pathlength (length solutionpath))
                    (setq currnode (getState (first solutionpath)))
                    (format t "~S " currnode)
                    (setq solutionpath (rest solutionpath))
           );end of dotimes
       );end of let
);end of printSolutionPath

;;;*************************************************
;;; Name : getHeuristic
;;; Description: Gets the evaluated heuristic from the node
;;; Parameters : node_heur - node being evaluated
;;; Return: Heuristic Number
;;;*************************************************
;;; Notes : The heuristic is the 4th element in the node
;;;*************************************************

(defun getHeuristic (node_heur)
       (return-from getHeuristic (nth 3 node_heur))
)

;;;*************************************************
;;; Name : getDepth
;;; Description: Gets the nodes depth in the search tree
;;; Parameters : node_depth - node being evaluated
;;; Return: Depth Integer
;;;*************************************************
;;; Notes : The depth is the 3rd element in the node
;;;*************************************************

(defun getDepth (node_depth)
       (return-from getDepth(nth 2 node_depth))
)
;;;*************************************************
;;; Name : getParent
;;; Description: Gets the node's parent state
;;; Parameters : node_parent - node being evaluated
;;; Return: State of the parent node
;;;*************************************************
;;; Notes : Parent is the 2nd element in the node
;;;*************************************************

(defun getParent (node_parent)
       (return-from getParent (nth 1 node_parent))
)

;;;*************************************************
;;; Name : getState
;;; Description: Gets the state represented in the given node
;;; Parameters : node_state - node being evaluated
;;; Return: State of the given node
;;;*************************************************
;;; Notes : State is the 1st element in the node
;;;*************************************************

(defun getState (node_state)
      (return-from getState (nth 0 node_state))
)

;;;*************************************************
;;; Name : createNode
;;; Description: Creates a 4-tuple list with the (new state, old state, new depth and heuristic)
;;;              It does it based on the node passed in
;;; Parameters : newState - represents the state of the newly created node
;;;              parentNode - its first element is the 2nd element of the new tuple
;;;              heuristic - the specific heuristic function for the
;;; Return: 4-tuple list to be added to the open list
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun createNode (newState parentNode heuristic)
       (return-from createNode (list newState (getState parentNode) (+ (getDepth parentNode) 1) (funcall heuristic newState (+ (getDepth parentNode) 1))))
)

;;;*************************************************
;;; Name : resetLists
;;; Description: sets the open and closed lists equal to NIL
;;; Parameters : N/A
;;; Return: N/A
;;;*************************************************
;;; Notes :
;;;*************************************************
(defun resetLists ()
       (setq *open* NIL)
       (setq *closed* NIL)
)

;;;*************************************************
;;; Name : addFrontList
;;; Description: Adds the given element to the front of the given list (cons)
;;; Parameters : element - value you want to add
;;;              list - what the element will be added to
;;; Return: List with the given node added to the front
;;;*************************************************
;;; Notes : Does not directly setq the inputed list.
;;;         Example Use: (setq listA (addFrontList element listA)
;;;*************************************************

(defun addFrontList (element_front list_front)
       (setq list_front (cons element_front list_front))
)

;;;*************************************************
;;; Name :
;;; Description:
;;; Parameters :
;;; Return:
;;;*************************************************
;;; Notes :
;;;*************************************************
