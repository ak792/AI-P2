
;should move tests to a different file
(defun test-searches ()
	(let ((search-algos (list  #'bfs
														#'ucs
														#'gbfs
														#'a*
														))
				(valid-mazes (list "tinyMaze.lay" 
													 "smallMaze.lay"
													 "mediumMaze.lay"
													 "openMaze.lay"
													 ))
				(invalid-mazes (list "goalUnreachableMaze.lay"
														 ))
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

(defun assert-maze-solution (maze-filename search-algo expected)
	(let* ((problem (load-maze-problem maze-filename))
				 (solution (funcall search-algo problem))
				 (res (is-valid-solution problem solution)))
		
		;(format t "~%~s solution: ~a" maze-filename solution)

		(if (not (eq expected res))
			(format t "~%TEST FAIL: ~s~%Solution: ~a~%Valid: ~a" maze-filename solution res))

		(eq expected res)))

(defun is-valid-solution (problem path)
	(not (null (is-valid-path problem (get problem 'start-state) path))))



(defun is-valid-path (problem state path)
;	(format t "~%state ~a path ~a" state path)

	(if (null path)
		(return-from is-valid-path))
		
	;if one away from goal
	(if (goal-test problem (result state (first path)))
		(return-from is-valid-path (result state (first path))))

	(and (is-valid-state problem (result state (first path)))
			 (is-valid-path problem (result state (first path)) (cdr path))))

;;if never finds the goal-state, returns nil
(defun bfs (problem)
	(let* ((explored (make-hash-table :test #'equal))
				(start-node (list (get problem 'start-state) nil 0))
				(frontier (list start-node)) ;should be a queue
				(curr-node)
				(curr-state)
				(curr-path)
				(adj-state)
				(child-node))  

		(do () 
			((eq (length frontier) 0) nil) 
			
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))

			(if (goal-test problem curr-state)
				(return-from bfs (reverse curr-path)))

			(setf (gethash curr-state explored) curr-state)

			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))


				(if (and (null (gethash adj-state explored)) ;;explored contains states
								 (null (find adj-node frontier :test #'equal))) ;;frontier contains nodes
					(progn
						(setf child-node (make-child curr-node adj-node))
						(setf frontier (append frontier (cons child-node nil)))
						(setf (gethash adj-state explored) adj-state) ;do we really need to hash it here?
						))))))


(defun ucs (problem)
	(let* ((explored (make-hash-table :test #'equal))
				(start-node (list (get problem 'start-state) nil 0))
				(frontier (list start-node))
				(curr-node)
				(curr-state)
				(curr-path)
				(adj-state)
				(child-node)
				(curr-node-in-frontier))  

		(do () 
			((eq (length frontier) 0) nil) 
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))
			
			(if (goal-test problem curr-state)
				(return-from ucs (reverse curr-path)))

			(setf (gethash curr-state explored) curr-state)


			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))

				(setf child-node (make-child curr-node adj-node))


				(setf curr-node-in-frontier (find adj-node frontier :test #'equal))
				(if (null (gethash adj-state explored)) ;;explored contains states
						(setf frontier (ins-repl frontier child-node #'third)))))))



(defun gbfs (problem)
	(let* ((explored (make-hash-table :test #'equal))
				(start-node (list (get problem 'start-state) nil 0))
				(frontier (list (append start-node (f-gbfs start-node (get problem 'goal-state))))) 
				(curr-node)
				(curr-state)
				(curr-path)
				(adj-state)
				(child-node))  

		(do () 
			((eq (length frontier) 0) nil) 
			
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))

			(if (goal-test problem curr-state)
				(return-from gbfs (reverse curr-path)))

			(setf (gethash curr-state explored) curr-state)
			
			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))

				(setf child-node (make-child curr-node adj-node))
				(setf child-node (append child-node (cons (f-gbfs child-node (get problem 'goal-state)) nil)))


				(setf curr-node-in-frontier (find adj-node frontier :test #'equal))
				(if (and (null (gethash adj-state explored)) (null curr-node-in-frontier)) ;;explored contains states
					;don't technically need to check the frontier for this particular problem
						(setf frontier (ins-repl frontier child-node #'fourth)))))))


(defun a* (problem)
	(let* ((explored (make-hash-table :test #'equal))
				(start-node (list (get problem 'start-state) nil 0))
				(frontier (list (append start-node (f-a* start-node (get problem 'goal-state))))) 
				(curr-node)
				(curr-state)
				(curr-path)
				(adj-state)
				(child-node))  

		(do () 
			((eq (length frontier) 0) nil) 
			
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))

			(if (goal-test problem curr-state)
				(return-from a* (reverse curr-path)))

			(setf (gethash curr-state explored) curr-state)
			
			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))

				(setf child-node (make-child curr-node adj-node))
				(setf child-node (append child-node (cons (f-a* child-node (get problem 'goal-state)) nil)))


				(setf curr-node-in-frontier (find adj-node frontier :test #'equal))
				(if (and (null (gethash adj-state explored)) (null curr-node-in-frontier)) ;;explored contains states
					;don't technically need to check the frontier for this particular problem
						(setf frontier (ins-repl frontier child-node #'fourth)))))))


(defun f-ucs (node)
	(g node))

(defun f-gbfs (node goal-state)
	(h goal-state (first node)))

(defun f-a* (node goal-state)
	(+ (g node) (h goal-state (first node))))

(defun g (node)
	(third node))

(defun h (goal-state state)
	(euclidean-dist state goal-state))

(defun euclidean-dist (state-1 state-2)
	(let ((x1 (first state-1))
				(x2 (first state-2))
				(y1 (second state-1))
				(y2 (second state-2)))
	(+ (* (- x1 x2) (- x1 x2))
		 (* (- y1 y2) (- y1 y2))))) 


;works
(defun test-ins-repl ()
	(let* (
				(the-list '((A nil 1) (B nil 2) (C nil 3) (D nil 4) (E nil 6)))
				(end-list '((A nil 1) (B nil 2) (C nil 3) (D nil 4)))
				(front-list '((A nil 7) (B nil 8) (C nil 83)))
				(one-elem-list '((A nil 6)))
				(one-elem-list-back '((A nil 3)))
				(two-elem-list '((A nil 4) (B nil 6)))
				(empty-list)
				(repl-the-list '((A nil 1) (B nil 2) (Z nil 3) (D nil 4) (E nil 6)))
				(repl-end-list '((A nil 1) (B nil 2) (C nil 3) (Z nil 4)))
				(repl-front-list '((Z nil 7) (B nil 8) (C nil 83)))
				(repl-one-elem-list '((Z nil 6)))
				(repl-two-elem-list '((Z nil 4) (B nil 6)))
				(res)
				(tests-res t)
				(lists (list the-list
										 end-list 
									   front-list
										 one-elem-list
										 one-elem-list-back 
										 two-elem-list 
									 	 empty-list
										 repl-the-list
										 repl-end-list
										 repl-front-list
										 repl-one-elem-list
										 repl-two-elem-list
										 ))
				(new-elem '(Z nil 3)))

		(dolist (curr-list lists)
			(setf res (ins-repl curr-list new-elem #'third))
			;(format t "~%~a" res)
			(if (or (null (is-sorted res #'third)) (null (no-duplicates res)))
				(progn
					(setf tests-res nil)
					(format t "~%TEST FAIL: input: ~a res: ~a " curr-list res))
			)
		)

		(format t "~%test-ins-repl res: ~a" tests-res)
		tests-res))


(defun ins-repl (succs s elem-num)
	(if (null succs)
		(return-from ins-repl (list s)))

	(if (equal (first (car succs)) (first s))
		(return-from ins-repl (copy-list succs)))

	(if (< (funcall elem-num (car succs)) (funcall elem-num s))
			(return-from ins-repl (cons (car succs) (ins-repl (cdr succs) s elem-num))))

	(cons s (remove s succs :test #'(lambda (n m) (equal (first n) (first m))))))

	
;works
(defun is-sorted (the-list elem-num)
	(if (or (null the-list) (null (cdr the-list)))
		(return-from is-sorted t))

	(if (<= (compare-frontier-nodes (first the-list) (second the-list) elem-num) 0)
		(is-sorted (cdr the-list) elem-num)
		nil))

;works
(defun no-duplicates (the-list)
	(no-duplicates-aux the-list (make-hash-table :test #'equal)))

(defun no-duplicates-aux (the-list contents)

	(if (null the-list)
		(return-from no-duplicates-aux t))

	(if (not (null (gethash (first (car the-list)) contents) ) )
		(return-from no-duplicates-aux nil))

		(setf (gethash (first (car the-list)) contents) (first (car the-list)))
		(no-duplicates-aux (cdr the-list) contents))

(defun test-no-duplicates ()
	(let* ((the-list '((A nil 1) (B nil 2) (C nil 3) (D nil 4) (E nil 6)))
				(end-list '((A nil 1) (B nil 2) (C nil 3) (D nil 4)))
				(front-list '((A nil 7) (B nil 8) (C nil 83)))
				(one-elem-list '((A nil 6)))
				(one-elem-list-back '((A nil 3)))
				(two-elem-list '((A nil 4) (B nil 6)))
				(empty-list)
				(tests-res t)
				(valid-lists (list the-list end-list front-list one-elem-list one-elem-list-back two-elem-list empty-list))
				 (invalid-lists (list (cons '(D nil 2) the-list)
															(cons '(E nil 2) the-list)
															(cons '(A nil 2) the-list)
															)))
				
		(dolist (curr-list valid-lists)
			(if (null (no-duplicates curr-list))
				(progn
					(setf tests-res nil)
					(format t "~%TEST FAIL: input: ~a " curr-list))))
		
		(dolist (curr-list invalid-lists)
			(if (not (null (no-duplicates curr-list)))
				(progn
					(setf tests-res nil)
					(format t "~%TEST FAIL: input: ~a no-duplicates: ~a" curr-list (no-duplicates curr-list)))))
		
		(format t "~%test-no-duplicates res: ~a" tests-res)
		tests-res))


;point to lower one
(defun compare-frontier-nodes (node-1 node-2 elem-num)
	(cond
		((< (funcall elem-num node-1) (funcall elem-num node-2)) -1) ;lower is first
		((= (funcall elem-num node-1) (funcall elem-num node-2)) 0)
		((> (funcall elem-num node-1) (funcall elem-num node-2)) 1))) ;lower is second










;;;loads a maze problem from a file
(defun load-maze-problem (filename)
	(format t "~%loading new maze problem ~a" filename)
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

		problem))


(defun successors (problem state)
	(let* ((action-cost 1)
				 (actions (list 'W 'E 'N 'S))
				 (west-state (result state 'W))
				(east-state (result state 'E))
				(north-state (result state 'N))
				(south-state (result state 'S))
				(possible-successors)
				(curr-successor)
				(successors-list))

		(dolist (action actions)
			(setf curr-successor (list (result state action)
																						action
																						action-cost))
			(if (is-valid-state problem (first curr-successor))
				(push curr-successor successors-list)))

		successors-list))

(defun is-valid-state (problem state)
	(not (eq #\% (aref (get problem 'maze) (second state) (first state)))))

(defun test-result ()
	(let ((tests-res t))

		(if (not (equal (result '(5 5) 'W) '(4 5)))
			(setf tests-res nil))

		(if (not (equal (result '(5 5) 'E) '(6 5)))
			(setf tests-res nil))

		(if (not (equal (result '(5 5) 'N) '(5 4)))
			(setf tests-res nil))

		(if (not (equal (result '(5 5) 'S) '(5 6)))
			(setf tests-res nil))
	
	tests-res))

(defun result (state action)
	(cond
		((eq action 'W) (list (1- (first state)) (second state)))
		((eq action 'E) (list (1+ (first state)) (second state)))
		((eq action 'N) (list (first state) (1- (second state))))
		((eq action 'S) (list (first state) (1+ (second state))))))

(defun make-child (node successor)
	(if (null node)
		(let ((child-node successor))
			(return-from make-child child-node))) 

	(let ((curr-state)
				(curr-path)
				(curr-path-cost)
				(child-state)
				(child-action)
				(child-action-cost)
				(full-child-path-cost)
				(full-child-path)
				(child-node))

		(setf curr-state (first node))
		(setf curr-path (second node))
		(setf curr-path-cost (third node))

		(setf child-state (first successor))
		(setf child-action (second successor))
		(setf child-action-cost (third successor))

		(setf full-child-path-cost (+ curr-path-cost child-action-cost))
		(setf full-child-path (cons child-action (copy-list curr-path))) 
		
		(setf child-node (list child-state 
													 full-child-path
													 full-child-path-cost))


		child-node))

(defun goal-test (problem state)
	(and (equal state (get problem 'goal-state))))

;only used for tests
(defun last-elem (the-list)
	(if (null (cdr the-list))
		(return-from last-elem (car the-list)))
	
	(last-elem (cdr the-list)))
