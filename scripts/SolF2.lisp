(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter no mooshack
; (load "datastructures.fas")
; (load "auxfuncs.fas")

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
		(print(states-to-list (auxdfs '() (problem-initial-state problem) (problem-fn-isGoal problem) (problem-fn-nextStates problem)  0 lim solution)))
		(auxdfs '() (problem-initial-state problem) (problem-fn-isGoal problem) (problem-fn-nextStates problem)  0 lim solution)
	)
 )

(defun teste (state stts)
	(loop for n in stts
		do(if  (equal (state-pos state) (state-pos n))
			(return-from teste nil))
	)
	T
)
		  
(defun auxdfs ( parents state isGoal nextStates depth limit  sol)
	(cond 
		;Is this a goal state state acording to the function we got from the problem?
		((funcall isGoal state)  (reverse sol))
		;see if weve reached the limit!
		((>= depth limit) nil)
		(t (progn(incf depth) 
			(loop for n in (funcall nextStates state)
					do(if (teste n parents)
						(let ((solution (auxdfs (cons state parents) n isGoal nextStates depth limit  (cons n sol))))
							(when solution (RETURN solution)))	
							
				)
			)
		))
))
	

(defun states-to-list (stts)
  (loop for st in stts
	  collect
	  (list	  (state-pos st)
		  (state-vel st)
		  (state-action st)
		  (state-cost st))))
		  
;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
;;parece ser facil de fazer fam
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
	 (let ((solution '()))
		(setf solution(cons  (problem-initial-state problem) solution))
	(auxdfs '() (problem-initial-state problem) (problem-fn-isGoal problem) (problem-fn-nextStates problem)  0 lim  solution)
	)
	)

	
