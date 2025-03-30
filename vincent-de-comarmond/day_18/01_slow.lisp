(defun split (input-string input-char &optional (accum '()))
  (let ((pos (position input-char input-string)))
    (cond ((= 0 (length input-string)) (nreverse
					(push "" accum)))
	  ((null pos) (nreverse
		       (push input-string accum)))
	  ('T (split (subseq input-string (1+ pos))
		     input-char
		     (push (subseq input-string 0 pos) accum))))))


(defun read-input (input-fp)
  (let ((instructions '()))
    (with-open-file (stream input-fp)
      (loop for line = (read-line stream 'nil) while line do
	(destructuring-bind (dir num colour) (split line #\space)
	  (push (list (char dir 0) (parse-integer num) colour) instructions))))
    (nreverse instructions)))


(defun rhr-complete-instructions (instructions)
  (let ((i 0)
	(j 0)
	(active '())
	(complete '()))

    (loop for instruction in instructions do
      (destructuring-bind (dir num col) instruction
	(declare (ignore col))

	(push (cons i j) complete)
	(dotimes (n num)
	  (case dir
	    (#\U (push (cons i (1+ j)) active))
	    (#\R (push (cons (1+ i) j) active))
	    (#\D (push (cons i (1- j)) active))
	    (#\L (push (cons (1- i) j) active)))

	  (case dir
	    (#\U (decf i))
	    (#\R (incf j))
	    (#\D (incf i))
	    (#\L (decf j)))
	  (push (cons i j) complete))))

    (setf active (remove-duplicates active :test #'equal))
    (setf active (set-difference active complete :test #'equal))
    (setf complete (reverse complete))

    (list active complete)))

(defun solve (active burnt)
  (let ((neighbours '((-1 . -1) (-1 . 0) (-1 . 1)
		      (0 . -1) (0 . 1)
		      (1 . -1) (1 . 0) (1 . 1))))

    (loop while (> (length active) 0) do
      (let ((current (pop active)))
	(loop for ngh in neighbours do
	  (let ((neighbour (cons (+ (car current) (car ngh))
				 (+ (cdr current) (cdr ngh)))))
	    (if (not (member neighbour burnt :test #'equal))
		(setf active (adjoin neighbour active :test #'equal)))))
	(setf burnt (adjoin current burnt :test #'equal))))

    (remove-duplicates burnt :test #'equal)))


(let* ((input-fp "./input.txt")
       (instructions (read-input input-fp)))
  (destructuring-bind (active burnt) (rhr-complete-instructions instructions)
    (setf burnt (time (solve active burnt)))

    (format t "Total burnt: ~s~%" (length burnt))))

;; 40664 is too low for part 1
;; 47417 is still too low for part 1

;; 52035 is the correct answer for part 1
;; 40 - 50 s ... horribly slow
