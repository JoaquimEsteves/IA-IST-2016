
;;; These functions, and any other ones needed must be implemented

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track";se calhar tenho de usar cdr e car na pos...
  (if(eq(aref(Track-env track) pos) nil) (return-from isObstaclep T))
  nil
)

(defun isGoalp (st) 
  "check if st is a goal state"
	(loop for e in (Track-endpositions (State-track st))
		do (if(eq e (State-position st)) (return-from isGoalp T))
	)
  nil
)

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (make-STATE :POS '(3 16)
	      :VEL '(1 3)
	      :ACTION act
	      :COST -100))


