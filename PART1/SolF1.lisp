;;; Utilizar estes includes para os testes na versao local
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter no mooshack
;(load "datastructures.fas")
;(load "auxfuncs.fas")


(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
	(if(eq(nth (car(cdr pos)) (nth (car pos)(Track-env track))) nil) 
		(return-from isObstaclep T))
	nil
)

(defun isGoalp (st) 
  "check if st is a goal state"
	(loop for e in (Track-endpositions (State-track st))
		do (if(equal  e (State-pos st)) (return-from isGoalp T))
	)
  nil
)

;Dadas duas listas como input, devolve uma lista de dois elementos.
;O primeiro elemento da lista de a devolver corresponde a uma soma dos respectivos primeiros elementos das listas de input,
;O segundo corresponde a uma soma dos respectivos segundos elementos das listas de input.
(defun addList (list1 list2)
	(list (+ (car list1)  (car list2)) (+ (car(cdr list1)) (car (cdr list2))))
)

(defun nextState (st act)
  "generate the nextState after state st and action act"
	(let((newVelocity  (addList (State-vel st) act)) (newPos )(newState))
	(setf newPos (addList  newVelocity  (State-pos st)))
	(setf newState
		(make-STATE :POS newPos
			  :VEL newVelocity
			  :ACTION act
			  :TRACK (State-track st)
			  :COST 0
		)
	)
	(cond ((isGoalp newState)
       (setf (State-cost newState) -100)
      ) ;_ end of the first then condition
       ((isObstaclep newPos (State-track st)) 
		(setf (State-cost newState) 20)
	   ) ;_ end of the second then condition  
	  (t (setf (State-cost newState) 1))  ;else,
	) ;_ end of cond statement
	newState			
	)
)


