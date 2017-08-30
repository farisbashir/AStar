;;;Test file
(load "L")
;(trace A*SearchRecursion A*Search)
;(traceAll)
;(resetLists)

;;Using Default Heuristic
(dribble "[zheth]3OUTA.txt")
(format t "~%8PUZZLE DEFAULT HEURISTIC~%");
(format t "~%EASY PUZZLE~%")
(A*Search *Easy8puzzleS* *8puzzleG* *8puzzle*)
(resetLists)
(format t "~%HARD PUZZLE~%")
(A*Search *Hard8puzzleS* *8puzzleG* *8puzzle*)
(resetLists)
;(format t "~%IMPOSSIBLE PUZZLE~%")
;(A*Search *Impossible8puzzleS* *8puzzleG* *8puzzle*)
;(resetLists)
(dribble)

;;Using Manhattan Heuristic
(dribble "[zheth]3OUTB.txt")
(format t "~%8PUZZLE MANHATTAN DISTANCE HEURISTIC~%");
(format t "~%EASY PUZZLE~%")
(A*Search *Easy8puzzleS* *8puzzleG* *8puzzleMH*)
(resetLists)
(format t "~%HARD PUZZLE~%")
(A*Search *Hard8puzzleS* *8puzzleG* *8puzzleMH*)
(resetLists)
;(format t "~%IMPOSSIBLE PUZZLE~%")
;(A*Search *Impossible8puzzleS* *8puzzleG* *8puzzleMH*)
;(resetLists)
(dribble)

;;Using Linear Conflict Heuristic
(dribble "[zheth]3OUTC.txt")
(format t "~%8PUZZLE LINEAR CONFLICT HEURISTIC~%");
(format t "~%EASY PUZZLE~%")
(A*Search *Easy8puzzleS* *8puzzleG* *8puzzleLC*)
(resetLists)
(format t "~%HARD PUZZLE~%")
(A*Search *Hard8puzzleS* *8puzzleG* *8puzzleLC*)
(resetLists)
;(format t "~%IMPOSSIBLE PUZZLE~%")
;(A*Search *Impossible8puzzleS* *8puzzleG* *8puzzleLC*)
;(resetLists)
(dribble)

(dribble "[zheth]3OUTD.txt")
(format t "~%RIVER CROSSING~%");
(A*Search *RiverS* *RiverG* *River*)
(resetLists)

(format t "~%~%GALLON~%")
(A*Search *GallonS* *GallonG* *Gallon*)
(resetLists)
(dribble)

;(untraceAll)
