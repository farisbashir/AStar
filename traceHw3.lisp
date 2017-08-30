(defun traceAll ()
       (traceA*Search)
       (traceRiver)
       (traceGallon)
       (trace8puzzle)
)

(defun untraceAll()
       (untraceA*Search)
       (untraceRiver)
       (untraceGallon)
       (untrace8puzzle)
)

(defun traceA*Search ()
       (trace
             A*Search
             A*SearchRecursion
             useMovesA*
             heuristicInsertSort
             findSolutionPath
             createNode
       )
)

(defun untraceA*Search ()
       (untrace
             A*Search
             A*SearchRecursion
             useMovesA*
             heuristicInsertSort
             findSolutionPath
             createNode
       )
)

(defun traceRiver ()
       (trace
              RiverCrossHeuristic
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
               RiverCrossHeuristic
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
              GallonHeuristic
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
              GallonHeuristic
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
             8puzzleHeuristic
             manhattanHeuristic
             linearConflictHeuristic
             moveup
             movedown
             moveleft
             moveright
       )
)

(defun untrace8puzzle ()
       (untrace
             findBlank
             8puzzleHeuristic
             manhattanHeuristic
             moveup
             movedown
             moveleft
             moveright
       )
)
