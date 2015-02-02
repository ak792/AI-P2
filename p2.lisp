


(defun run-search-tests ()
	(let ((search-algos (list; #'bfs
														#'ucs))
				(valid-mazes (list "tinyMaze.lay" 
													 ;"smallMaze.lay" "mediumMaze.lay" "openMaze.lay"
													 ))
				(invalid-mazes (list ;"goalUnreachableMaze.lay"
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

;;currently only bfs
(defun assert-maze-solution (maze-filename search-algo expected)
	(let* ((problem (load-maze-problem maze-filename))
				 (solution (funcall search-algo problem))
				 (res (is-valid-path problem solution)))
		
		;(format t "~%~s solution: ~a" maze-filename solution)

		(if (not (eq expected res))
			(format t "~%TEST FAIL: ~s~%Solution: ~a~%Valid: ~a" maze-filename solution res))

		(eq expected res)))

(defun is-valid-path (problem path)
	(let ((start-state-path (first path))
				(start-state-problem (get problem 'start-state))
				(goal-state-path (last-elem path))
				(goal-state-problem (get problem 'goal-state)))
		(if (not (and (eq-states-2 start-state-path start-state-problem)
						 			(eq-states-2 goal-state-path goal-state-problem)))
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

	t)

;;if never finds the goal-state, returns nil
(defun bfs (problem)
	(let* ((explored (make-hash-table :test 'equal))
				(start-node (list (get problem 'start-state) (list (get problem 'start-state)) 0))
				(frontier (list start-node)) ;should be a queue
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

		(do ((i 0 (1+ i))) 
			((or (eq i -1) (eq (length frontier) 0)) nil) ;replace i with eq 1 for testing
			
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))
			(setf curr-path-cost (third curr-node))

			(if (goal-test problem curr-state)
				(return-from bfs (reverse curr-path)))

			(setf (gethash (format nil "~s" curr-state) explored) curr-state)

			(dolist (adj-node (successors problem curr-state))
				(setf adj-state (first adj-node))
				(setf adj-action (second adj-node))
				(setf adj-action-cost (third adj-node))


				(if (and (null (gethash (format nil "~s" adj-state) explored)) ;;explored contains states
								 (null (find adj-node frontier :test #'equal))) ;;frontier contains nodes
					(progn
						(setf full-adj-path (cons adj-action (copy-list curr-path)))
						(setf full-adj-path-cost (+ curr-path-cost adj-action-cost))
						(setf child-node (list adj-state 
																	 full-adj-path
																	 full-adj-path-cost))
						
						(setf frontier (append frontier (cons child-node nil)))
						(setf (gethash (format nil "~s" adj-state) explored) adj-state))))))) ;do we really need to hash it here?

(defun ucs (problem)
	(let* ((explored (make-hash-table :test 'equal))
				(start-node (list (get problem 'start-state) (list (get problem 'start-state)) 0))
				(frontier (list start-node)) ;sorted list. really should be a queue
				(curr-node)
				(curr-state)
				(curr-path)
				(curr-path-cost)
				(adj-state)
				(adj-action)
				(adj-action-cost)
				(full-adj-path)
				(full-adj-path-cost)
				(child-node)
				(curr-node-in-frontier))  

	;	(format t "starting ucs with start-node ~a" start-node)

		(do ((i 0 (1+ i))) 
			((or (eq i 1) (eq (length frontier) 0)) nil) ;replace i with eq 1 for testing
;			(format t "~%getting new node. frontier: ~a" frontier)
			(setf curr-node (pop frontier))

			(setf curr-state (first curr-node))
			(setf curr-path (second curr-node))
			(setf curr-path-cost (third curr-node))
			
;			(format t "~%parsing new node: ~%curr-node ~a, curr-state ~a" curr-node curr-state)


			(if (goal-test problem curr-state)
				(return-from ucs (reverse curr-path)))

;			(format t "~%post goal-test")

			(setf (gethash (format nil "~s" curr-state) explored) curr-state)


;			(format t "~%put new node into hash table")
			
			(dolist (adj-node (successors problem curr-state))
;				(format t "here")
				(setf adj-state (first adj-node))
				(setf adj-action (second adj-node))
				(setf adj-action-cost (third adj-node))
	;			(format t "here2")

				(setf full-adj-path-cost (+ curr-path-cost adj-action-cost))
				(setf full-adj-path (cons adj-action (copy-list curr-path))) ;inefficient. should really do only if needed
				(setf child-node (list adj-state 
															 full-adj-path
															 full-adj-path-cost))

				;probably need to change the find function test
				;need to insert into frontier at right spot, replacing if needed
				
				;if not in explored or frontier, add to frontier

;				(format t "~%child-node: ~a" child-node)
				(format t "~%checking adj node. frontier: ~a" frontier)
				(setf curr-node-in-frontier (find adj-node frontier :test #'equal))
				(if (and (null (gethash (format nil "~s" adj-state) explored)) ;;explored contains states
								 (null curr-node-in-frontier)) ;;frontier contains nodes
					(progn
						(format t "~% test insert: ~a" (insert-into-frontier frontier child-node))
						(setf frontier (insert-into-frontier frontier child-node))
			;			(setf (gethash (format nil "~s" adj-state) explored) adj-state)
						(format t "~% was not in frontier or explored. added to frontier: ~a" frontier)
					)

					;if in frontier and child-node has a lower path cost, replace curr-node-in-frontier with child-node
					;else
					(progn
						(if (and (not (null curr-node-in-frontier))
										 (> (compare-frontier-node-costs curr-node-in-frontier child-node) 0)
								)
							;replace frontier with a new one with curr-node-in-frontier removed and child-node added
							(progn
								(setf frontier (remove curr-node-in-frontier frontier :test #'equal))
								(setf frontier (insert-into-frontier frontier child-node))
							)
						)
					)
				)

				
			)
		)

		(format t "~%reached end of ucs")
	)
)

;works
(defun run-tests-insert-into-frontier ()
	(let* ((the-list '((nil nil 1) (nil nil 2) (nil nil 3) (nil nil 4) (nil nil 6)))
				(end-list '((nil nil 1) (nil nil 2) (nil nil 3) (nil nil 4)))
				(front-list '((nil nil 7) (nil nil 8) (nil nil 83)))
				(one-elem-list '((nil nil 6)))
				(one-elem-list-back '((nil nil 3)))
				(two-elem-list '((nil nil 4) (nil nil 6)))
				(empty-list)
				(res)
				(tests-res t)
				(lists (list the-list
										 end-list front-list
										 one-elem-list one-elem-list-back two-elem-list empty-list
										 ))
				(new-elem '(nil nil 5))
				)
				
		(dolist (curr-list lists)
			(setf res (insert-into-frontier curr-list new-elem))
		;	(format t "~%~a: ~a" (is-sorted res) res)
			(if (null (is-sorted res))
				(progn
					(setf tests-res nil)
					(format t "~%TEST FAIL: input: ~a res: ~a " curr-list res))))

		(format t "~%tests-insert-into-frontier res: ~a" tests-res)
		tests-res))


;update to handle the actual contents that will be in the frontier
;change the < operator


(defun insert-into-frontier (the-list elem)
	(if (null the-list)
		(return-from insert-into-frontier elem))

	(if (null elem)
		(return-from insert-into-frontier the-list))

	(format t "insert ~a into frontier ~a" elem the-list)
	(if (< (compare-frontier-node-costs elem (first the-list)) 0)
		(cons elem the-list)
		(insert-into-frontier-aux the-list elem)))

(defun insert-into-frontier-aux (the-list elem)
	(format t "here")
	(let ((res))
		(if (null the-list)
			(return-from insert-into-frontier-aux))

		
	(format t "here")
		(format t "~%the-list ~a ~a ~a" the-list elem (and (not (null elem)) (null (second the-list))))
		
		
		(if (and (not (null elem)) 
						 (or (null (second the-list)) 
								 (< (compare-frontier-node-costs elem (second the-list)) 0 )))
			(progn
				(format t "~%if evaluated to T")
				(setf res (cons (car the-list) (cons elem (insert-into-frontier-aux (cdr the-list) nil))))
			)
			(progn
				(format t "~%if evaluted to nil")
				(setf res (cons (car the-list) (insert-into-frontier-aux (cdr the-list) elem))))
			)
		
		res))

(defun is-sorted (the-list)
	(if (or (null the-list) (null (cdr the-list)))
		(return-from is-sorted t))

	(if (< (compare-frontier-node-costs (first the-list) (second the-list)) 0)
		(is-sorted (cdr the-list))
		nil))

;point to lower one
(defun compare-frontier-node-costs (node-1 node-2)
	(format t "~%comparing costs ~a ~a" node-1 node-2)
	(cond
		((< (third node-1) (third node-2)) -1) ;lower is first
		((= (third node-1) (third node-2)) 0)
		((> (third node-1) (third node-2)) 1))) ;lower is second


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
	(let* ((left-state (list (1- (first state)) (second state)))
				(right-state (list (1+ (first state)) (second state)))
				(up-state (list (first state) (1- (second state))))
				(down-state (list (first state) (1+ (second state))))
				(possible-successors (list left-state right-state up-state down-state))
				(successors-list))

		(dolist (possible-successor possible-successors)
			(if (is-valid-successor problem possible-successor)
				(push (build-successor possible-successor) successors-list)))

		successors-list))

(defun is-valid-successor (problem state)
	(not (eq #\% (aref (get problem 'maze) (second state) (first state)))))

(defun build-successor (state)
	(let ((action-cost 1))
		(list (copy-list state) ;can get away with copying only once and using the original for the rest
					(copy-list state)
					action-cost)))


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
	(and (eq-states-2 state (get problem 'goal-state))))

;wrapper for eq-states-list
;eventually should refactor eq-states out (generic functions?)
;need to make list the + operator that can take unlimited arguments
(defun eq-states-2 (state1 state2)
	(eq-states-list (list state1 state2)))

;generic eq-states
(defun eq-states-list (states)
	(let ((base-state (first states)))
		(dolist (state states)
			(if (not (and (eq (first state) (first base-state))
										(eq (second state) (second base-state))))
				(return-from eq-states-list)
			)
		)
		
		t))

(defun last-elem (the-list)
	(if (null (cdr the-list))
		(return-from last-elem (car the-list)))
	
	(last-elem (cdr the-list)))
