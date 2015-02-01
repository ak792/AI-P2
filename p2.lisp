


(defun run-tests ()
	(let ((search-algos (list #'bfs))
				(valid-mazes (list "tinyMaze.lay" "smallMaze.lay" "mediumMaze.lay" "openMaze.lay"))
				(invalid-mazes (list "goalUnreachableMaze.lay"))
				(tests-res t)
				(test-success)
				(expected-res))
		
		(dolist (search-algo search-algos)
		
			(setf expected-res t)
			(dolist (maze valid-mazes)
				(setf test-success (assert-maze-solution maze search-algo expected-res))
				(if (null test-success)
					(setf tests-res nil)))

			(setf expected-res nil)
			(dolist (maze invalid-mazes)
				(setf test-success (assert-maze-solution maze search-algo expected-res))
				(if (null test-success)
					(setf tests-res nil))))


		(format t "~%tests res: ~a" tests-res)

		tests-res))

;;currently only bfs
(defun assert-maze-solution (maze-filename search-algo expected)
	(let* ((problem (load-maze-problem maze-filename))
				 (solution (funcall search-algo problem))
				 (res (is-valid-path problem solution)))
		
		;(format t "~%~s solution: ~a" maze-filename solution)

		(if (not (eq expected res))
			(format t "~%TEST FAIL: ~s~%Solution: ~a~%Valid: ~a" filename solution res))

		(eq expected res)))

;should eventually take a list
(defun eq-states (state-1 state-2)
	(and (eq (first state-1) (first state-2))
			 (eq (second state-1) (second state-2))))

(defun last-elem (the-list)
	(if (null (cdr the-list))
		(return-from last-elem (car the-list)))
	
	(last-elem (cdr the-list)))

(defun is-valid-path (problem path)
	(let ((start-state-path (first path))
				(start-state-problem (get problem 'start-state))
				(goal-state-path (last-elem path))
				(goal-state-problem (get problem 'goal-state)))
		(if (not (and (eq-states start-state-path start-state-problem)
						 			(eq-states goal-state-path goal-state-problem)))
			(return-from is-valid-path))) 
		
	(dolist (action path)
		(let ((len-x (second (array-dimensions (get problem 'maze))))
					(len-y (first (array-dimensions (get problem 'maze))))
					(action-x (first action))
					(action-y (second action)))
			(if (or (>= action-x len-x)
							(< action-x 0)
							(>= action-y len-y)
							(< action-y 0)
							(eq #\% (aref (get problem 'maze) action-y action-x)))
				(return-from is-valid-path))))

	t
)


;;;WRITE TESTS
;;;(load "p2.lisp") (setf *problem* (load-maze-problem "tinyMaze.lay")) (bfs *problem*)

;;if never finds the goal-state, returns nil
(defun bfs (problem)
	(let* ((explored (make-hash-table :test 'equal))
				(start-node (list (get problem 'start-state) (list (get problem 'start-state)) 0))
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

	;	(format t "~%start state: ~a~%~agoal state" (get problem 'start-state) (get problem 'goal-state))
	;	(format t "~%frontier: ~a" frontier)
		(do ((i 0 (1+ i))) ;no variable updating
			((or (eq i -1) (eq (length frontier) 0)) nil) ;replace i with eq 1 for testing
			
		;	(format t "~% iteration ~a" i)
			;can't put all this business in the do bc need the termination condition checked first. (although pop would just be nil so could probably put the curr-node there at least)
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))
			(setf curr-path-cost (third curr-node))

	;		(format t "~%currnode ~a" curr-node)
	
	;;the hash table isn't working

			(if (goal-test problem curr-state)
				(return-from bfs (reverse curr-path)))

		;	(format t "~%curr-state ~a to string ~s" curr-state (format nil "~s" curr-state))
		;	(format t "~%curr-state~a pre hash: ~a" curr-state (gethash (format nil "~s" curr-state) explored))

		;	(setf (gethash 'a explored) t)
		;	(format t "~%testhash ~a" (gethash 'a explored))

			(setf (gethash (format nil "~s" curr-state) explored) curr-state)
		;	(format t "~%curr-state post hash: ~a" (gethash (format nil "~s" curr-state) explored))

	;		(format t "~%successors: ~a" (successors problem curr-state))

			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))
				(setf adj-action (second adj-node))
				(setf adj-action-cost (third adj-node))

		;		(format t "~%adj-state ~a adj-state hash ~a" adj-state (gethash adj-state explored))

				(if (and (null (gethash (format nil "~s" adj-state) explored)) ;;explored contains states
								 (null (find adj-node frontier :test #'equal))) ;;frontier contains nodes
					(progn
						(setf full-adj-path (cons adj-action (copy-list curr-path)))
						(setf full-adj-path-cost (+ curr-path-cost adj-action-cost))
						(setf child-node (list adj-state 
																	 full-adj-path
																	 full-adj-path-cost))
						(if (null frontier)
							(setf frontier (cons child-node nil))
							(setf frontier (append frontier (cons child-node nil)))
						)
					;	(format t "~%frontier: ~a" frontier)

						(setf (gethash (format nil "~s" adj-state) explored) adj-state)
	;	(format t "~%frontier: ~a" frontier)
					)
				)
			)
		;	(format t "~%curr-state: ~a~%frontier: ~a" curr-state frontier)
		)
	)
)


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

		
;		(format t "~%~a" (get problem 'maze))
;		(format t "~%Start: ~a" (get problem 'start-state))
;		(format t "~%Goal: ~a" (get problem 'goal-state))

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
