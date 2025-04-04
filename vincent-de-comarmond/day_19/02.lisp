(defun split (input-string input-char &optional (accum '()))
  (let ((pos (position input-char input-string)))
    (cond ((= 0 (length input-string)) (nreverse
					(push "" accum)))
	  ((null pos) (nreverse
		       (push input-string accum)))
	  ('T (split (subseq input-string (1+ pos))
		     input-char
		     (push (subseq input-string 0 pos) accum))))))


(defun make-rule (input-string)
  (let ((lines (mapcar (lambda (x) (split x #\:))
		       (split input-string #\,))))
    (loop for rule in lines
	  collect (if (or (position #\< (car rule))
			  (position #\> (car rule)))
		      (list (char (car rule) 1)
			    (char (car rule) 0)
			    (parse-integer (subseq (car rule) 2))
			    (cadr rule))
		      rule))))


(defun parse-workflow (input-line)
  (let* ((pos (position #\{ input-line))
	 (name (subseq input-line 0 pos))
	 (rule (subseq input-line (1+ pos) (1- (length input-line)))))
    (list name (make-rule rule))))


(defun read-input (input-fp)
  (let ((workflows (make-hash-table :size 10000 :test #'equal)))

    (with-open-file (stream input-fp)
      (loop for line = (read-line stream 'nil)
	    while (and line (not (equal line ""))) do
	      (destructuring-bind (name rule) (parse-workflow line)
		(setf (gethash name workflows) rule))))
    workflows))


(defun solve-step (universal-part workflows)
  (let ((current-rule (nth (getf universal-part :idx)
			   (gethash (getf universal-part :rule) workflows)))
	(current-idx (getf universal-part :idx)))

    (if (= 1 (length current-rule))
	(progn (setf (getf universal-part :rule) (car current-rule))
	       (setf (getf universal-part :idx) 0)
	       (list universal-part)) ;; return-from ???

	(destructuring-bind (op char_ val nxt) current-rule
	  (let* ((copy1 (copy-seq universal-part))
		 (copy2 (copy-seq universal-part))
		 (lower-sym (case char_
			      (#\x ':x0)
			      (#\m ':m0)
			      (#\a ':a0)
			      (#\s ':s0)))
		 (lower-val (getf universal-part lower-sym))
		 (upper-sym (case char_
			      (#\x ':x1)
			      (#\m ':m1)
			      (#\a ':a1)
			      (#\s ':s1)))
		 (upper-val (getf universal-part upper-sym)))

	    (if (and (<= lower-val val)
		     (<= val upper-val))
		(case op
		  (#\<
		   (setf (getf copy1 upper-sym) (1- val))
		   (setf (getf copy1 :rule) nxt)
		   (setf (getf copy1 :idx) 0)
		   (setf (getf copy2 lower-sym) val)
		   (setf (getf copy2 :idx) (1+ current-idx))
		   (list copy1 copy2)) ;; return-from ??
		  (#\>
		   (setf (getf copy1 upper-sym) val)
		   (setf (getf copy1 :idx) (1+ current-idx))
		   (setf (getf copy2 lower-sym) (1+ val))
		   (setf (getf copy2 :rule) nxt)
		   (setf (getf copy2 :idx) 0)
		   (list copy1 copy2)) ;; return-from ??
		  )
		(progn (setf (getf universal-part :idx) (1+ current-idx))
		       (list universal-part))))))))


(defun calculate-generic-accepted-parts (workflows)
  (let ((rejected-parts '())
	(accepted-parts '())
	(active (list (list :x0 1 :x1 4000
			    :m0 1 :m1 4000
			    :a0 1 :a1 4000
			    :s0 1 :s1 4000
			    :rule "in" :idx 0))))

    (loop while (> (length active) 0) do
      (let* ((current (pop active))
	     (next-gen (solve-step current workflows)))

	(loop for next-step in next-gen do
	  (cond ((equal "R" (getf next-step :rule))
		 (push next-step rejected-parts))
		((equal "A" (getf next-step :rule))
		 (push next-step accepted-parts))
		('T (push next-step active))))))
    accepted-parts))


(defun compute-possibilities (generic-accepted-parts)
  (let ((total 0))
    (loop for generic-part in generic-accepted-parts do
      (destructuring-bind (&key x0 x1 m0 m1 a0 a1 s0 s1 &allow-other-keys) generic-part
	(setf total (+ total (* (- (1+ x1) x0)
				(- (1+ m1) m0)
				(- (1+ a1) a0)
				(- (1+ s1) s0))))))
    total))


(time (let* ((input-fp "./input.txt")
	     (workflows (read-input input-fp))
	     (generic-accepted-parts (calculate-generic-accepted-parts workflows))
	     (possibilities (compute-possibilities generic-accepted-parts)))
	(format t "Number possibilities: ~s~%" possibilities)))

;; 432427 is the right answer for part 1
;; run time is 0.000616 seconds of total run time (0.000599 user, 0.000017 system)

;; 143760172569135 is the right answer for part 2
;; run time is 0.001081 seconds of total run time (0.001000 user, 0.000081 system)
