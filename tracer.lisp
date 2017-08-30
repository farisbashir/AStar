(defun traceAll ()
       (traceDBFS)
       (traceArthGS)
       (traceRiver)
       (traceGallon)
       (trace8puzzle)
       (traceHelper)
)
(defun untraceAll()
       (untraceDBFS)
       (untraceArthGS)
       (untraceRiver)
       (untraceGallon)
       (untrace8puzzle)
       (untraceHelper)
)

(defun traceDBFS ()
       (trace
          DFS
          DFSRecursion
          BFS
          BFSRecursion
          addFrontOfOpenList
          addClosedList
          printListLengths
          printSolutionPath
          useMovesDFS
          useMovesBFS
          getDepth
          resetLists
          findSolutionPath
          findParent
      )
)
(defun untraceDBFS ()
       (untrace
          DFS
          DFSRecursion
          BFS
          BFSRecursion
          addFrontOfOpenList
          addClosedList
          printListLengths
          printSolutionPath
          useMovesDFS
          useMovesBFS
          addSolutionPath
          getDepth
          resetLists
          findSolutionPath
          findParent
      )
)

(defun traceHelper ()
       (trace
            createNode
            updateNode
            getState
       )
)

(defun untraceHelper ()
       (untrace
             createNode
             updateNode
             getState
       )
 )

(defun traceRiver ()
       (trace
              eastWolf
              westWolf
              eastGoat
              westGoat
              eastCabbage
              westCabbage
              farmerToggle
              farmermove
              currVriverlose
        )
)

(defun untraceRiver ()
       (untrace
               eastWolf
               westWolf
               eastGoat
               westGoat
               eastCabbage
               westCabbage
               farmerToggle
               farmermove
               currVriverlose
       )
)

(defun traceGallon ()
       (trace
              dump3
              dump5
              refill3
              refill5
              pour3to5
              pour5to3
       )
)

(defun untraceGallon ()
       (untrace
              dump3
              dump5
              refill3
              refill5
              pour3to5
              pour5to3
       )
)

(defun trace8puzzle ()
       (trace
             findBlank
             moveup
             movedown
             moveleft
             moveright
       )
)

(defun untrace8puzzle ()
       (untrace
             findBlank
             moveup
             movedown
             moveleft
             moveright
       )
)

(defun traceArthGS ()
       (trace currVgoal)
       (trace power2)
       (trace mult7)
       (trace addRandom)
       (trace add1)
 )

 (defun untraceArthGS ()
        (untrace currVgoal)
        (untrace power2)
        (untrace mult7)
        (untrace addRandom)
        (untrace add1)
  )
