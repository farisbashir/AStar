;GLOBAL VARIABLES
(defvar *open* ())
(defvar *closed* ())
(defvar *temp* ())
(defvar *gamepath* ())
(defvar *puzzle1* '(CallFCROSS CallFWCROSS CallFGCROSS CallFCCROSS))                ;contains the move for the farmer puzzle
(defvar *puzzle2* '(CallRefill5 CallRefill3 CallDump5 CallDump3 Call5to3 Call3to5))
(defvar *puzzle3* '(moveUp moveDown moveLeft moveRight))


(defun CallAStar (nodes goal moves)
    (AStar nodes goal moves)
    (print *goalpath*)
)

(defun AStar (nodes goal moves)
    (format t "node~s~%" nodes)
    (COND
        ( (equal goal (first nodes))
          (format t "Open List: ~s~%" *open*)
          (format t "Closed List: ~s~%" *closed*)
          (format t "Solution Path: ")
          (return-from AStar (goalpath nodes))
        )
        ( (equal NIL nodes)
          (setq *open* (rest *open*))
          (AStar (first *open*) goal moves)
        )
        ( (or (equal (first nodes) NIL) (equal 20 (getDepth nodes))) ;if the first node is NIL or the depth is 20
            (setq *open* (rest *open*)) ;skip the node and update the open list
            (AStar (first *open*) goal moves) ;call recursion function
        )
        ( T
          (let ((copymoves moves) (movelength (length moves)) )
              (setq *open* (rest *open*))
              (loop while(> movelength 0) do
                  (COND
                      ( (equal (length *open*) 0)
                      ;  (print (first copymoves))
                        (setq *open* (CONS (funcall (first copymoves) nodes) *open*))
                      ;  (print *open*)
                        (COND
                            ( (equal (first *open*) NIL)
                              (setq *open* ())
                            )
                        )
                      )
                      ( T
                      ;  (print (first copymoves))
                        (setq *open* (sorts (funcall (first copymoves) nodes) NIL *open*) )
                      ;  (print *open*)
                      )
                  )
                  (setq copymoves (rest copymoves)) ;remove the move from the list and update
                  (setq movelength (- movelength 1)) ;subtract movelength
              )
          )
          (setq *closed* (cons nodes *closed*))
          (AStar (first *open*) goal moves)
        )
    )
)

(defun goalpath (goalnode)
    (let ((temp nil) (curr goalnode))
        (loop while(> (first(reverse curr)) 0) do
;(format t "GamePath: ~S~%~%" *gamepath*)
            (setq temp (first *closed*))
            (COND
                ( (equal (LENGTH *closed*) 2)
                  (setq *gamepath* (CONS (first(reverse *closed*)) *gamepath*))
                  (setq curr temp)
                )
                ( (equal (first(reverse temp)) (- (first(reverse curr)) 1))
                  (COND
                    ( (equal (first temp) (first (rest curr)))
                      (setq *gamepath* (CONS curr *gamepath*))
                      (setq curr temp)
                    )
                  )
                )
            )
            (setq *closed* (rest *closed*))
            (setq temp (first *closed*))
        )
    )
    (let ((temp (first *gamepath*)) (temp2 (first (rest *gamepath*))))
        (setq *gamepath* (rest (rest *gamepath*)))
        (setq *gamepath* (CONS temp2 (CONS temp *gamepath*)))
    )
    (setq *gamepath* (reverse *gamepath*))
    (return-from goalpath *gamepath*)
)

(defun sorts (node front back) ; sorts might be fucking with my list
    (let ( (temp (getState (first *open*))) (copylist *open*) (val (length *open*)) )
        (loop while (> val 0) do
            (COND
                ( (equal (getState node) temp)
                  (return-from sorts *open*)
                )
                ( T
                  (setq copylist (rest copylist))
                  (setq temp (getState (first copylist)))
                )
            )
            (setq val (- val 1))
        )
    )
    (let ( (temp (getState (first *closed*))) (copylist *closed*) (val (length *closed*)) )
        (loop while (> val 0) do
            (COND
                ( (equal (getState node) temp)
                  (return-from sorts *open*)
                )
                ( T
                  (setq copylist (rest copylist))
                  (setq temp (getState (first copylist)))
                )
            )
            (setq val (- val 1))
        )
    )
    (COND
        ( (equal node NIL)
          (return-from sorts *open*)
        )
        ( (equal (first node) NIL)
          (return-from sorts *open*)
        )
    )
    (loop while(> (heuristic node) (heuristic (first back))) do
        (setq front (reverse (cons (first back) (reverse front))))
        (setq back (rest back))
        (COND
            ( (equal (heuristic (first back)) NIL)
              (return)
            )
        )
    )
    (setq front (reverse (cons node (reverse front))))
    (setq front (append front back))
    (return-from sorts front)
)

(defun heuristic (node)
    (let ((temp (first (reverse (first node)))))
        (return-from heuristic temp)
    )
)

(defun getState (node)
    (return-from getState (first node))
)

(defun createNode (newState node)
       (return-from createNode (list newState (first node) (+ (first (reverse node)) 1))) ;returns a list created by combining the child state, parent state, and 1+ on the depth
)

(defun CalcFPH (node)
    (let ((temp (getState node)) (value 0))
        (COND
            ( (equal node NIL)
              (return-from CalcFPH NIL)
            )
            ( (equal (first temp) 1)
              (setq value (+ value 1))
            )
        )
        (COND
            ( (equal (first (rest temp)) 1)
              (setq value (+ value 1))
            )
        )
        (COND
            ( (equal (first (rest (rest temp))) 1)
              (setq value (+ value 1))
            )
        )
        (COND
            ( (equal (first (rest (rest (rest temp)))) 1)
              (setq value (+ value 1))
            )
        )
        (return-from CalcFPH value)
    )
)

(defun CalcWJH (node)
    (let ((temp (getState node)) (value 0))
        (COND
            ( (equal node NIL)
              (return-from CalcWJH NIL)
            )
            ( (equal (first temp) 0)
              (setq value 4)
            )
            ( (equal (first temp) 1)
              (setq value 3)
            )
            ( (equal (first temp) 2)
              (setq value 2)
            )
            ( (equal (first temp) 3)
              (setq value 1)
            )
            ( (equal (first temp) 5)
              (setq value 1)
            )
            ( (equal (first temp) 4)
              (setq value 0)
            )
        )
        (return-from CalcWJH value)
    )
)

(defun Calc8PH (node)
    (let ((temp (getState node)) (value 0))
        (COND
            ( (equal node NIL)
              (return-from Calc8PH NIL)
            )
            ( (not (equal (first temp) 1))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest temp)) 2))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest temp))) 3))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest (rest temp)))) 4))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest (rest (rest temp))))) 5))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest (rest (rest (rest temp)))))) 6))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest (rest (rest (rest (rest temp))))))) 7))
              (setq value (+ value 1))
            )
        )
        (COND
            ( (not (equal (first (rest (rest (rest (rest (rest (rest (rest temp)))))))) 8))
              (setq value (+ value 1))
            )
        )
        ;(setq value (+ value (getDepth node)))
        (return-from Calc8PH value)
    )
)

(defun SetFPH (node)
    (COND
      ( (equal (first node) NIL)
        NIL
      )
      ( T
        (let ((newNode node))
            (setq newNode (reverse (CONS (CalcFPH node) (rest (reverse (first node))))))
            (setq node (CONS newNode (rest node)))
            (return-from SetFPH node)
        )
      )
    )
)

(defun SetWJH (node)
    (COND
        ( (equal (first node) NIL)
          NIL
        )
        ( T
          (let ((newNode node))
              (setq newNode (reverse (CONS (CalcWJH node) (rest (reverse (getState node))))))
              (setq node (CONS newNode (rest node)))
              (return-from SetWJH node)
          )
        )
    )
)

(defun Set8PH (node)
    (COND
        ( (equal (first node) NIL)
          NIL
        )
        ( T
          (let ((newNode (getState node)))
            (setf (nth 9 newNode) (Calc8PH node))
            (setq node (CONS newNode (rest node)))
            (return-from Set8PH node)
          )
        )
    )
)

(defun getDepth (node)
    (return-from getDepth (first (reverse node)))
)
;*****************************FARMER MOVES*********************************

(defun F-CROSSES (state)
  (let ((temp NIL))
      (COND
        ( (equal (first (rest (reverse state))) 1)
          (setq temp (first (reverse state)))
          (setq state (reverse (CONS temp (CONS 0 (rest (rest (reverse state))) ) ) ) )
        )
        ( (equal (first (rest (reverse state))) 0)
          (setq temp (first (reverse state)))
          (setq state (reverse (CONS temp (CONS 1 (rest (rest (reverse state))) ) ) ) )
        )
        ( T (return-from F-CROSSES NIL))
      )
  )
)

(defun CallFCROSS (nodes)
  (let ((currstate NIL) (newnode NIL)) ;currstate is used for the child node, newnode is the newly created node
    (setq currstate (F-CROSSES (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallFCROSS (SetFPH newnode))
  )
)

(defun FW-CROSSES (state)
  (COND
    ( (equal (first state) 1)
      (COND
        ( (equal (first (rest (reverse state))) 0)
          NIL
        )
        ( T
          (setq state (CONS 0 (rest state)))
          (setq state (F-CROSSES state))
        )
      )
    )
    ( (equal (first state) 0)
      (COND
        ( (equal (first (rest (reverse state))) 1)
          NIL
        )
        ( T
          (setq state (CONS 1 (rest state)))
          (setq state (F-CROSSES state))
        )
      )
    )
    ( T (return-from FW-CROSSES NIL))
  )
)

(defun CallFWCROSS (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (FW-CROSSES (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallFWCROSS (SetFPH newnode))
  )
)

(defun FG-CROSSES (state)
  (let ((temp NIL))
    (COND
      ( (equal (first (rest state)) 1)
        (COND
          ( (equal (first (rest (reverse state))) 0)
            NIL
          )
          (T
            (setq temp (first state))
            (setq state (CONS temp (CONS 0 (rest (rest state)))))
            (setq state (F-CROSSES state))
          )
        )
      )
      ( (equal (first (rest state)) 0)
        (COND
          ( (equal (first (rest (reverse state))) 1)
            NIL
          )
          ( T
            (setq temp (first state))
            (setq state (CONS temp (CONS 1 (rest (rest state)))))
            (setq state (F-CROSSES state))
          )
        )
      )
      ( T (return-from FG-CROSSES NIL))
    )
  )
)

(defun CallFGCROSS (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (FG-CROSSES (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallFGCROSS (SetFPH newnode))
  )
)

(defun FC-CROSSES (state)
  (let ((temp1 NIL) (temp2 NIL))
    (COND
      ( (equal (first (rest (rest state))) 1)
        (COND
          ( (equal (first (rest (reverse state))) 0)
            NIL
          )
          (T
            (setq temp1 (first state))
            (setq temp2 (first (rest state)))
            (setq state (CONS temp1 (CONS temp2 (CONS 0 (rest (rest (rest state)))))))
            (setq state (F-CROSSES state))
          )
        )
      )
      ( (equal (first (rest (rest state))) 0)
        (COND
          ( (equal (first (rest (reverse state))) 1)
            NIL
          )
          ( T
            (setq temp1 (first state))
            (setq temp2 (first (rest state)))
            (setq state (CONS temp1 (CONS temp2 (CONS 1 (rest (rest (rest state)))))))
            (setq state (F-CROSSES state))
          )
        )
      )
      ( T (return-from FC-CROSSES NIL))
    )
  )
)

(defun CallFCCROSS (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (FC-CROSSES (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallFCCROSS (SetFPH newnode))
  )
)

;*****************************WATER JUG MOVES*********************************

;;;*************************************************
;;; Name : Refill5
;;; Description: changes the state of the 5 gal jug to 5
;;; Parameters : state: contains the start state
;;;*************************************************
(defun Refill5 (state)
    (COND
        ( (equal (first state) 5)
          (return-from Refill5 NIL)
        )
        ( T
          (setq state (CONS 5 (rest state)))
        )
    )
)

;;;*************************************************
;;; Name : CallRefill5
;;; Description: calls Refill5 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun CallRefill5 (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (Refill5 (first nodes)))
    (setq newnode (createNode currstate nodes))
    (return-from CallRefill5 (SetWJH newnode))
  )
)

;;;*************************************************
;;; Name : Refill3
;;; Description: changes the state of the 3 gal jug to 3
;;; Parameters : state: contains the start state
;;;*************************************************
(defun Refill3 (state)
    (COND
      ( (equal (first(rest state)) 3)
        (return-from Refill3 NIL)
      )
      ( T
        (let (temp (first (reverse state)))
          (setq state (reverse (CONS temp (CONS 3 (rest (rest (reverse state)))))))
        )
      )
    )
)

;;;*************************************************
;;; Name : CallRefill3
;;; Description: calls Refill3 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun CallRefill3 (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (Refill3 (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallRefill3 (SetWJH newnode))
  )
)

;;;*************************************************
;;; Name : Dump5
;;; Description: changes the state of the 5 gal jug to 0
;;; Parameters : state: contains the start state
;;;*************************************************
(defun Dump5 (state)
    (COND
      ( (equal (first state) 0)
        (return-from Dump5 NIL)
      )
      ( T
        (setq state (CONS 0 (rest state)))
      )
    )
)

;;;*************************************************
;;; Name : CallDump5
;;; Description: calls Dump5 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun CallDump5 (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (Dump5 (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallDump5 (SetWJH newnode))
  )
)

;;;*************************************************
;;; Name : Dump3
;;; Description: changes the state of the 3 gal jug to 0
;;; Parameters : state: contains the start state
;;;*************************************************
(defun Dump3 (state)
    (COND
      ( (equal (first(rest state)) 0)
        (return-from Dump3 NIL)
      )
      ( T
        (let (temp (first (reverse state)))
          (setq state (reverse (CONS temp (CONS 0 (rest (rest (reverse state)))))))
        )
      )
    )
)

;;;*************************************************
;;; Name : CallDump3
;;; Description: calls Dump3 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun CallDump3 (nodes)
  (let ((currstate NIL) (newnode NIL))
    (setq currstate (Dump3 (first nodes)) )
    (setq newnode (createNode currstate nodes))
    (return-from CallDump3 (SetWJH newnode))
  )
)

;;;*************************************************
;;; Name : 5to3
;;; Description: transfers as much units from the 3 gal
;;;              to the 5 gal
;;; Parameters : state: contains the start state
;;;*************************************************
(defun 5to3 (state)
    (COND
      ( (equal (first state) 0)
        (return-from 5to3 NIL)
      )
      ( (equal (first (rest state)) 3)
        (return-from 5to3 NIL)
      )
      ( T
        ( let ((5gal (first state)) (3gal (first (rest state))))
          (loop while (and (> 5gal 0) (< 3gal 3)) do
              (setq 5gal (- 5gal 1))
              (setq 3gal (+ 3gal 1))
          )
          (setq state (CONS 5gal (CONS 3gal (rest (rest state)))))
        )
      )
    )
    (return-from 5to3 state)
)

;;;*************************************************
;;; Name : Call5to3
;;; Description: calls 5to3 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun Call5to3 (nodes)
  (let ((currstate NIL) (newnode NIL) (temp NIL))
    (setq temp (copy-tree nodes))
    (setq currstate (5to3 (first nodes)) )
    (setq newnode (createNode currstate temp))
    (return-from Call5to3 (SetWJH newnode))
  )
)

;;;*************************************************
;;; Name : 3to5
;;; Description: transfers as much units from the 5 gal
;;;              to the 3 gal
;;; Parameters : state: contains the start state
;;;*************************************************
(defun 3to5 (state)
    (COND
        ( (equal (first state) 5)
          (return-from 3to5 NIL)
        )
        ( (equal (first (rest state)) 0)
          (return-from 3to5 NIL)
        )
        ( T
          ( let ((5gal (first state)) (3gal (first (rest state))))
            (loop while(and (< 5gal 5) (> 3gal 0)) do
                (setq 5gal (+ 5gal 1))
                (setq 3gal (- 3gal 1))
            )
            (setq state (CONS 5gal (CONS 3gal (rest (rest state)))))
          )
        )
    )
    (return-from 3to5 state)
)

;;;*************************************************
;;; Name : Call3to5
;;; Description: calls 5to3 and creates a new node
;;; Parameters : nodes: contains the start node
;;;*************************************************
(defun Call3to5 (nodes)
  (let ((currstate NIL) (newnode NIL) (temp NIL))
    (setq temp (copy-tree nodes))
    (setq currstate (3to5 (first temp)) )
    (setq newnode (createNode currstate nodes))
    (return-from Call3to5 (SetWJH newnode))
  )
)


;*****************************8 PUZZLE MOVES*********************************

(defun findEmpty (node)
    (COND
        ( (equal (getState node) NIL)
          NIL
        )
        ( T
          (let ((copystate (getState node)) (index 0) )
              (loop while (not (equal 'X (first copystate))) do
                  (setq copystate (rest copystate))
                  (setq index (+ index 1))
              )
              (return-from findEmpty index)
          )
        )
    )
)

(defun moveUp (node)
    (let ( (index (findEmpty node)) (copystate (copy-tree (getState node))))
        (COND
            ( (< index 6)
              (setf (nth index copystate) (nth (+ index 3) copystate))
              (setf (nth (+ index 3) copystate) 'X)
              (return-from moveUp (Set8PH (createNode copystate node)))
            )
            (T
              (return-from moveUp NIL)
            )
        )
    )
)

(defun moveDown (node)
    (let ( (index (findEmpty node)) (copystate (copy-tree (getState node))))
        (COND
            ( (> index 2)
              (setf (nth index copystate) (nth (- index 3) copystate))
              (setf (nth (- index 3) copystate) 'X)
              (return-from moveDown (Set8PH (createNode copystate node)))
            )
            (T
              (return-from moveDown NIL)
            )
        )
    )
)

(defun moveRight (node)
    (let ( (index (findEmpty node)) (copystate (getState (copy-tree node))))
        (COND
            ( (> (mod index 3) 0)
              (setf (nth index copystate) (nth (- index 1) copystate))
              (setf (nth (- index 1) copystate) 'X)
              (return-from moveRight (Set8PH (createNode copystate node)))
            )
            (T
              (return-from moveRight NIL)
            )
        )
    )
)

(defun moveLeft (node)
    (let ( (index (findEmpty node)) (copystate (getState (copy-tree node))))
        (COND
            ( (< (mod index 3) 2)
              (setf (nth index copystate) (nth (+ index 1) copystate))
              (setf (nth (+ index 1) copystate) 'X)
              (return-from moveLeft (Set8PH (createNode copystate node)))
            )
            (T
              (return-from moveLeft NIL)
            )
        )
    )
)
