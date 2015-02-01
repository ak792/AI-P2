;;;loads a maze problem from a file
;;;(load "p2.lisp") (load-maze-problem "tinyMaze.lay")
;;;(load "p2.lisp") (load-maze-problem "smallMaze.lay")
;;;(load "p2.lisp") (load-maze-problem "mediumMaze.lay")
;;;(load "p2.lisp") (load-maze-problem "openMaze.lay")

(defun load-maze-problem (filename)
	(let ((istream (open filename :direction :input :if-does-not-exist nil))
				(x-dimen)
				(y-dimen)
				(maze)
				(problem)
				(curr-input)
				(curr-char)
				(start-location)
				(goal-location))
		
		;;return if filename was invalid
		(if (null (streamp istream))
			(progn
				(format t "could not open file ~s~%" filename)
				(return-from load-maze-problem)))


		;error: i was passing in make-array as x,y but it expects y,x
		(setf x-dimen (read istream nil 'eof))
		(setf y-dimen (read istream nil 'eof))
		;;reads in values of maze		
		(setf maze (make-array (list y-dimen x-dimen)))


		(do ((curr-input (read-line istream nil 'eof) (read-line istream nil 'eof))
				 (row 0 (1+ row)))
			((or (eq curr-input 'eof) (eq row y-dimen)) 'done) ;termination condition
			
			;;parses and adds each char in curr-input to the maze
			(dotimes (col x-dimen)
				(setf (aref maze row col)
					(case (char curr-input col)
						;error: locations were being set as y,x not x,y
						(#\S 
							 (setf start-location (list col row))
							 #\S)
						(#\G
								(setf goal-location (list col row))
								#\G)
						(otherwise (char curr-input col)))))) 
		(close istream)
		
		(setf (get problem 'maze) maze)
		(setf (get problem 'start-state) start-location)
		(setf (get problem 'goal-state) goal-location)

		
		(format t "~%~a" (get problem 'maze))
		(format t "~%Start: ~a" (get problem 'start-state))
		(format t "~%Goal: ~a" (get problem 'goal-state))

		problem))

	;aref takes it as y,x

;;;(load "p2.lisp") (setf *problem* (load-maze-problem "smallMaze.lay")) (successors *problem* (get *problem* 'start-state)) 

;state vs action
;what are successors...
(defun successors (problem state)
	;;; if down is not %, add to list. etc
	(let ((successors-list)
				(action-cost 1))
		;left
		(if (not (eq (aref (get problem 'maze) (second state) (1- (first state))) #\%))
			(push (list
							(list (1- (first state)) (second state))
							(list (1- (first state)) (second state)) ;go to... how is this different from state??
							action-cost) 
				successors-list))

		;right
		(if (not (eq (aref (get problem 'maze) (second state) (1+ (first state)))  #\%))
				(push (list
							(list (1+ (first state)) (second state))
							(list (1+ (first state)) (second state)) ;go to... how is this different from state??
							action-cost) 
				successors-list))

		;up
		(if (not (eq (aref (get problem 'maze) (1- (second state)) (first state)) #\%))
			(push (list
							(list (first state) (1- (second state)))
							(list (first state) (1- (second state))) ;go to... how is this different from state??
							action-cost) 
				successors-list))

		;down
		(if (not (eq (aref (get problem 'maze) (1+ (second state)) (first state)) #\%))
			(push (list
							(list (first state) (1+ (second state)))
							(list (first state) (1+ (second state))) ;go to... how is this different from state??
							action-cost) 
				successors-list))

		;returns successors is (x,y) format
		successors-list))

;;;(load "p2.lisp") (setf *problem* (load-maze-problem "smallMaze.lay")) (setf *successors* (successors *problem* (get *problem* 'start-state))) (setf *curr-child* (make-child nil (first *successors*))) (setf *successors* (successors *problem* (first *curr-child*)))

(defun make-child (node successor)
	(if (null node)
		(let ((child-node successor))
			(return-from make-child child-node))) 

	(let ((child-node node))
		
		(setf (first child-node) (first successor))
		(append (second successor) (second child-node))
		(setf (third child-node) (+ (third child-node) (third successor)))

		child-node))

(defun goal-test (problem state)
	(and (eq (first (get problem 'goal-state)) (first state)) 
			 (eq (second (get problem 'goal-state)) (second state))))


;;;(load "p2.lisp") (setf *problem* (load-maze-problem "smallMaze.lay")) (bfs *problem*)

;;if never finds the goal-state, returns nil
(defun bfs (problem)
	(let* ((explored ())
				(start-node (list (get problem 'start-state) (list) 0))
				(frontier (list start-node)) ;really should be a queue
				(curr-node)
				(curr-state)
				(curr-path)
				(curr-path-cost)
				(adj-state)
				(adj-action)
				(adj-action-cost)
				(full-adj-path)
				(full-adj-path-cost)
				(child-node))  


	;	(format t "~%frontier: ~a" frontier)
		(do ((i 0 (1+ i))) ;no variable updating
			((eq (length frontier) 0) 'done)
			
			(format t "~% iteration ~a" i)
			;can't put all this business in the do bc need the termination condition checked first. (although pop would just be nil so could probably put the curr-node there at least)
			(setf curr-node (pop frontier))
			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))
			(setf curr-path-cost (third curr-node))

	;		(format t "~%currnode ~a" curr-node)
	
	;;the explored hash table isn't working

			(if (goal-test problem curr-state)
				(return-from bfs curr-path))

	;		(format t "~%curr-state~a pre hash: ~a" curr-state (gethash curr-state explored))

			(push curr-state explored)
	;		(setf (gethash curr-state explored) t)
	;		(format t "~%curr-state post hash: ~a" (gethash curr-state explored))

	;		(format t "~%successors: ~a" (successors problem curr-state))

			(dolist (adj-node (successors problem curr-state)) ;ensure this is optimized and isn't calling successors every iteration
				(setf adj-state (first adj-node))
				(setf adj-action (second adj-node))
				(setf adj-action-cost (third adj-node))

		;		(format t "~%adj-state ~a adj-state hash ~a" adj-state (gethash adj-state explored))


		;find isn't working properly. probably because lists of lists
				(if (and (null (find adj-state explored)) ;;explored contains states
								 (null (find adj-node frontier))) ;;frontier contains nodes
					(progn
						(setf full-adj-path (append (copy-list curr-path) adj-action))
						(setf full-adj-path-cost (+ curr-path-cost adj-action-cost))
						(setf child-node (list adj-state 
																	 full-adj-path
																	 full-adj-path-cost))
						(if (null frontier)
							(setf frontier (cons child-node nil))
							(setf frontier (append frontier (cons child-node nil)))
							)
	;	(format t "~%frontier: ~a" frontier)
					)
				)
			)
		)
	)
)
