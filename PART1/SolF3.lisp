(load "datastructures.lisp")
(load "auxfuncs.lisp")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))

;; Solution of phase 2

;;; Pedir 

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  ;;fazer next state das accoes
  ;;devolver essa lista
	(let ((possible-states ) )
		(loop for act in (possible-actions)
			do (push (nextState st act) possible-states)
		)
			
	possible-states
))
	

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
	(setf cutoff? '())
	(let ((solution '()))
		(setf solution(cons  (problem-initial-state problem) solution))
		; (print(states-to-list (auxdfs '() (problem-initial-state problem) (problem-fn-isGoal problem) (problem-fn-nextStates problem)  0 lim solution)))
		(auxdfs '() (problem-initial-state problem) (problem-fn-isGoal problem) (problem-fn-nextStates problem)  0 lim solution cutoff?)
	)
 )

(defun checkIfVisited (state stts) ;CHANGE THE NAME OF THIS FUNCTION PLZ
	(loop for n in stts
		do(if(equal state n)
			(return-from checkIfVisited nil)))
	T
)
		  
(defun auxdfs ( parents state isGoal nextStates depth limit  sol cutoff?)
	(cond 
		;Is this a goal state state acording to the function we got from the problem?
		((funcall isGoal state)  (reverse sol))
		;see if weve reached the limit!
		((>= depth limit) (progn 
			(setf cutoff? ':CORTE)
			(return-from auxdfs nil)
		))
		(t (progn(incf depth)
			; (let ((check '())) 
			(loop for n in (funcall nextStates state)
					do(if (checkIfVisited n parents)
						(let ((solution (auxdfs (cons state parents) n isGoal nextStates depth limit  (cons n sol) cutoff?)))
							(when solution (if(not (eq solution ':CORTE)) (return-from auxdfs solution))))	
							
				)
			)
			(return-from auxdfs cutoff?)
			; )
		))
))
		  
;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
;;parece ser facil de fazer fam
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
	(loop for x from 0 to lim
		do(let ((solution  ( limdepthfirstsearch problem x)))
		(unless (or (eq solution nil) (eq solution ':CORTE)) (RETURN solution)))
	)	 
)
;; Solution of phase 3

(defun smallestcolumn (endpos)	
	(setf mincolumn (nth 1 (nth 0 endpos)))
	(loop for n in endpos
	  do(if (> mincolumn (nth 1 n))
			(setf mincolumn  (nth 1 n))
		)
	)
	mincolumn
)

;; Heuristic
(defun compute-heuristic (st)
	(let ((track(state-track st)) (columin(smallestcolumn(track-endpositions track))))
		(if (getTrackContent  (state-pos st) (state-track st))
			(return-from compute-heuristic (- columin (nth 1 (state-pos st))))
			(return-from compute-heuristic most-positive-fixnum)
		)
	)
)
	  
  
(defun create-node(state parent gscore fscore)
	(make-node :state state :parent parent :g gscore :f fscore)
)


(defun reconstruct-path(node)
	(let ((totalpath '()) current-node)
		(setf current-node node)
		(push (node-state current-node) totalpath)
		(loop while (node-parent current-node) do
			(setf current-node (node-parent current-node))
			(push (node-state current-node) totalpath)
		)

	totalpath
	)
)

(defun fscore-min(stack)
  (let ((stack-ax stack)(minfscore most-positive-fixnum) node-corrente node-aux)
	(loop while stack-ax do
		(setf node-corrente(pop stack-ax))
		(if( < (node-f node-corrente) minfscore)
			(progn
				(setf minfscore (node-f node-corrente))
				(setf node-aux node-corrente)
			)
		)
	)
	node-aux
  )
)

(defun stack-return(stack node)
	(let ( (stack-aux '()) node-aux)
		(loop while stack do
			(setf node-aux (pop stack))
			(if (not (equalp node-aux node) )
				(setf stack-aux(nconc stack-aux (list node-aux)))
			)
		)
		stack-aux
	)
)
;;; A*
(defun a* (problem)
      (let ((stack '()) current-node node-aux gscore-aux)
			(setf current-node (create-node (problem-initial-state problem) NIL 0 0))
			(push current-node stack)
			(loop while stack do
				(setf current-node (fscore-min stack))
				(if (funcall (problem-fn-isGoal problem) (node-state current-node))
					(return-from a* (reconstruct-path current-node) )
					(progn
						(dolist (states (funcall (problem-fn-nextStates problem) (node-state current-node)))
								(setf node-aux (create-node states current-node most-positive-fixnum most-positive-fixnum))
								(setf gscore-aux (state-cost (node-state node-aux)))
								(if (< gscore-aux (node-g node-aux))
									(progn
										(setf (node-g node-aux) gscore-aux)
										(setf (node-f node-aux) (+ gscore-aux (funcall (problem-fn-h problem) (node-state node-aux))))
										(push node-aux stack)
									)
								)
						)
						(setf stack (stack-return stack current-node))
					)
				)
				   
			)
            
		)

  )
  
 (defun best-search (problem)
 nil)