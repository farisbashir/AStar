;;;*************************************************
;;;; Zachary Heth
;;;; Professor Binsted
;;;; Homework 2
;;;; Helper Methods
;;;*************************************************
;;; Notes : All the functions take the entire node as a parameter, not the state
;;;         These are just extra functions I use in all the other files
;;;*************************************************

;;;*************************************************
;;; Name : createNode
;;; Description: Creates the updated node that will be returned to the search
;;; Parameters : newstate - the new state of the node
;;;              node_create - the parent node
;;;*************************************************
;;; Notes : Increases the depth by 1
;;;*************************************************

(defun createNode (newState node_create)
       (return-from createNode (list newState (getState node_create) (+ (first (reverse node_create)) 1)))
)

;;;*************************************************
;;; Name : updateNode
;;; Description: changes the state (1st element) of the node
;;; Parameters : node_update - node to be updated
;;;*************************************************
;;; Notes : Used in confunction with the toggle type functions
;;;         It does not update the depth, or parent
;;;*************************************************

(defun updateNode (newState node_update)
       (return-from updateNode (cons newstate (rest node_update)))
)

;;;*************************************************
;;; Name : getState
;;; Description: Returns the state of the node (the first element of the tuple)
;;; Parameters : node_state - node with the state to be returned
;;;*************************************************
;;; Notes :
;;;*************************************************

(defun getState (node_state)
        (return-from getState (first node_state))
);end of getState
