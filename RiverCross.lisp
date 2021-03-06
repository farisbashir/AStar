;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 2
;;;; River Crossing State Space Representations
;;;*************************************************
;;; Notes : All the functions take the entire node as a parameter, not the state
;;;*************************************************

;;; PROBLEM/GOAL : A farmer want to cross the river with a wolf, goat, and cabbage
;;;                If the wolf and goat, or goat and cabbage are left alone, we lose
;;; RULES        : The farmer can take one across at a time, or can choose to cross alone

(defvar *riverstart* '(F W G C)) ;start state
(defvar *rivergoal* '(∆ ∆ ∆ ∆)) ;end state
(defvar *river* '(eastWolf westWolf eastGoat westGoat eastCabbage westCabbage farmerToggle)) ;moves
(defvar *riverlose* '((F ∆ ∆ C) (F W ∆ ∆) (∆ W G ∆) (∆ ∆ G C)))

;;;*************************************************
;;; Name : currVriverlose
;;; Description: Compares the current state to the losing states
;;;              Returns NIL if they are equal, T otherwise
;;; Parameters : curr_lose - node with state to compare to the losing states
;;;*************************************************
;;; Notes : To make it generic, make the losing list a parameter
;;;*************************************************

(defun currVriverlose (curr_lose)
       (let ((currstate (getState curr_lose)) (loselist *riverlose*) (lose nil) )
            (dotimes (x (length loselist))                             ;loop for the # of losing states
                      (setq lose (first loselist))                     ;set lose to the first losing state
                      (cond ((equal currstate lose)                    ;if the current state is a losing state
                                    (return-from currVriverlose NIL)) ;return false
                            (T (setq loselist (rest loselist))))       ;otherwise prepare for next iteration
            );end of dotimes
       );end of let
       (return-from currVriverlose T)                                 ;return T otherwise
);end of currVriverlose

;;;*************************************************
;;; Name : farmermove
;;; Description: Toggles the F into a ∆ and viceversa
;;;              This represents the farmer crossing sides without taking anything
;;; Parameters : curr_move - node with state that will be toggled
;;;*************************************************
;;; Notes : Returns an updated node, not a new node
;;;*************************************************

(defun farmermove (curr_move)
      (let ((curr (copy-tree curr_move)) (copy (copy-tree curr_move)) (newstate nil))
           (setq newstate (getState curr))
           (cond ((equal (nth 0 newstate) 'F)                       ;if farmer is on the east side
                         (setf (nth 0 newstate) '∆))                 ;move him to the west side
                 (T (setf (nth 0 newstate) 'F))                     ;otherwise move him to the east side
           );end of cond
           (return-from farmermove (updateNode newstate copy))
      );end of let
);end of farmermove

;;;*************************************************
;;; Name : farmerToggle
;;; Description: Toggles the F into a ∆ and viceversa
;;;              This represents the farmer crossing sides without taking anything
;;; Parameters : curr_move - node with state that will be toggled
;;;*************************************************
;;; Notes : Returns an new node, not an updatednode
;;;*************************************************

(defun farmerToggle (curr_togg)
       (cond ((currVriverlose curr_togg)
              (let ((curr (copy-tree curr_togg)) (copy (copy-tree curr_togg)) (newstate nil) (newnode nil))
                   (setq newstate (getState curr))
                   (cond ((equal (nth 0 newstate) 'F)                       ;if farmer is on the east side
                                 (setf (nth 0 newstate) '∆))                 ;move him to the west side
                         (T (setf (nth 0 newstate) 'F))                     ;otherwise move him to the east side
                   );end of cond
                   (return-from farmerToggle (createNode newstate copy))
              );end of let
              )
              (T NIL))
);end of farmerToggle

;;;*************************************************
;;; Name : wolfmove
;;; Description: Toggles the W into a ∆ and viceversa.
;;;              Returns the new state, does not return a new node
;;; Parameters : curr_wmove - the node with state that will be toggled
;;;*************************************************
;;; Notes : This is used within the methods east/westWolf
;;;         It does not check the context in which the move is made.
;;;         It simply toggles it
;;;*************************************************

(defun wolfmove (curr_wmove)
       (let ((curr (copy-tree curr_wmove)) (copy (copy-tree curr_wmove))(newstate nil))
            (setq newstate (getState curr))
            (cond ((equal (nth 1 newstate) 'W)                       ;if wolf is on the east side
                          (setf (nth 1 newstate) '∆))                 ;move him to the west side
                  (T (setf (nth 1 newstate) 'W))                     ;otherwise move him to the east side
            );end of cond
            (return-from wolfmove (updateNode newstate copy))
       );end of let
);end of wolfmove

;;;*************************************************
;;; Name : goatmove
;;; Description: Same as wolf move, except for the goat
;;; Parameters : curr_gmove - the node with state that will be toggled
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun goatmove (curr_gmove)
       (let ((curr (copy-tree curr_gmove)) (copy (copy-tree curr_gmove)) (newstate nil))
            (setq newstate (getState curr))
            (cond ((equal (nth 2 newstate) 'G)                       ;if goat is on the east side
                          (setf (nth 2 newstate) '∆))                 ;move him to the west side
                  (T (setf (nth 2 newstate) 'G))                     ;otherwise move him to the east side
            );end of cond
            (return-from goatmove (updateNode newstate copy))
       );end of let
);end of goatmove

;;;*************************************************
;;; Name : cabbagemove
;;; Description: Same as wolf move, except for the cabbage
;;; Parameters : curr_gmove - the node with state that will be toggled
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun cabbagemove (curr_cmove)
       (let ((curr (copy-tree curr_cmove)) (copy (copy-tree curr_cmove)) (newstate nil))
            (setq newstate (getState curr))
            (cond ((equal (nth 3 newstate) 'C)                       ;if cabbage is on the east side
                          (setf (nth 3 newstate) '∆))                 ;move him to the west side
                  (T (setf (nth 3 newstate) 'C))                     ;otherwise move him to the east side
            );end of cond
            (return-from cabbagemove (updateNode newstate copy))
       );end of let
);end of cabbagemove

;;;*************************************************
;;; Name : farmereast
;;; Description: returns T if the farmer is on the east side of the river
;;;                      NIL otherwise
;;; Parameters : curr_FE - node with state that is being evaluated
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun farmereast (curr_FE)
       (if (equal (first (getState curr_FE)) '∆)
           (return-from farmereast T))
       (return-from farmereast NIl)
);end of farmereast

;;;*************************************************
;;; Name : farmerwest
;;; Description: returns T if the farmer is on the west side of the river
;;;                      NIL otherwise
;;; Parameters : curr_FE - node with state that is being evaluated
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun farmerwest (curr_FW)
       (if (equal (first (getState curr_FW)) 'F)
           (return-from farmerwest T))
       (return-from farmerwest NIl)
);end of farmerwest

;;;*************************************************
;;; Name : eastWolf
;;; Description: Moves the wolf and farmer from the west side to the east side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_EWolf - node evaluated
;;;*************************************************
;;; Notes : Relies on farmerwest, wolfmove, and farmermove
;;;*************************************************

(defun eastWolf (curr_EWolf)
        (cond ((and (currVriverlose curr_EWolf)(farmerwest curr_EWolf) (member 'W (getState curr_EWolf)))      ;if farmer is on west side of the river
                  (let ((newstate (getState (wolfmove (farmermove curr_EWolf)))))   ;
                      (return-from eastWolf                                         ;move the wolf to the east side (create new node)
                              (createNode newstate curr_EWolf))))
              (T NIL))                             ;else return nil
);end of eastWolf

;;;*************************************************
;;; Name : westWolf
;;; Description: Moves the wolf and farmer from the east side to the west side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_WWolf - node evaluated
;;;*************************************************
;;; Notes : Relies on farmereast, wolfmove, and farmermove
;;;*************************************************

(defun westWolf (curr_WWolf)
        (cond ((and (currVriverlose curr_WWolf) (farmereast curr_WWolf) (not (member 'W (getState curr_WWolf))))      ;if farmer is on east side of the river
                  (let ((newstate (getState (wolfmove (farmermove curr_WWolf)))))   ;
                      (return-from westWolf                                         ;move the wolf to the west side (create new node)
                              (createNode newstate curr_WWolf))))
              (T NIL))                             ;else return nil
);end of westWolf

;;;*************************************************
;;; Name : eastGoat
;;; Description: Moves the goat and farmer from the west side to the east side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_EGoat - node evaluated
;;;*************************************************
;;; Notes : Relies on farmerwest, goatmove, and farmermove
;;;*************************************************

(defun eastGoat (curr_EGoat)
        (cond ((and (currVriverlose curr_EGoat) (farmerwest curr_EGoat) (member 'G (getState curr_EGoat)))      ;if farmer is on west side of the river
                  (let ((newstate (getState (goatmove (farmermove curr_EGoat)))))   ;
                      (return-from eastGoat                                         ;move the goat to the east side (create new node)
                              (createNode newstate curr_EGoat))))
              (T NIL))                             ;else return nil
);end of eastGoat

;;;*************************************************
;;; Name : westGoat
;;; Description: Moves the goat and farmer from the east side to the west side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_WGoat - node evaluated
;;;*************************************************
;;; Notes : Relies on farmereast, goatmove, and farmermove
;;;*************************************************

(defun westGoat (curr_WGoat)
        (cond ((and (currVriverlose curr_WGoat) (farmereast curr_WGoat) (not (member 'G (getState curr_WGoat))))      ;if farmer is on east side of the river
                  (let ((newstate (getState (goatmove (farmermove curr_WGoat)))))   ;
                      (return-from westGoat                                         ;move the Goat to the west side (create new node)
                              (createNode newstate curr_WGoat))))
              (T NIL))                             ;else return nil
);end of westGoat

;;;*************************************************
;;; Name : eastcabbage
;;; Description: Moves the cabbage and farmer from the west side to the east side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_ECabbage - node evaluated
;;;*************************************************
;;; Notes : Relies on farmerwest, cabbagemove, and farmermove
;;;*************************************************

(defun eastCabbage (curr_ECabbage)
        (cond ((and (currVriverlose curr_ECabbage) (farmerwest curr_ECabbage) (member 'C (getState curr_ECabbage)))      ;if farmer is on west side of the river
                  (let ((newstate (getState (cabbagemove (farmermove curr_ECabbage)))))   ;
                      (return-from eastCabbage                                         ;move the Cabbage to the east side (create new node)
                              (createNode newstate curr_ECabbage))))
              (T NIL))                             ;else return nil
);end of eastCabbage

;;;*************************************************
;;; Name : westCabbage
;;; Description: Moves the Cabbage and farmer from the east side to the west side of the river
;;;              It checks if the move is allowed before doing it. Returns the new node, or NIL if not allowed
;;; Parameters : curr_WCabbage - node evaluated
;;;*************************************************
;;; Notes : Relies on farmereast, cabbagemove, and farmermove
;;;*************************************************

(defun westCabbage (curr_WCabbage)
        (cond ((and (currVriverlose curr_WCabbage) (farmereast curr_WCabbage) (not (member 'C (getState curr_WCabbage))))      ;if farmer is on east side of the river
                  (let ((newstate (getState (cabbagemove (farmermove curr_WCabbage)))))   ;
                      (return-from westCabbage                                         ;move the Cabbage to the west side (create new node)
                              (createNode newstate curr_WCabbage))))
              (T NIL))                             ;else return nil
);end of westCabbage
