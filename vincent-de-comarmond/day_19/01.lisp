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
    #'(lambda (x)
	(let ((output 'nil))
	  (loop for line in lines while (not output) do
	    (if (= 1 (length line))
		(setf output (car line))
		(destructuring-bind (cond_ result) line
		  (let ((operator (if (eql (char cond_ 1) #\<)
				      (lambda (a b) (< a b))
				      (lambda (a b) (> a b))))
			(number (parse-integer cond_ :start 2)))

		    (case (char cond_ 0)
		      (#\x (if (funcall operator (getf x :x) number)
			       (setf output result)))
		      (#\m (if (funcall operator (getf x :m) number)
			       (setf output result)))
		      (#\a (if (funcall operator (getf x :a) number)
			       (setf output result)))
		      (#\s (if (funcall operator (getf x :s) number)
			       (setf output result))))))))
	  output))))


(defun parse-workflow (input-line)
  (let* ((pos (position #\{ input-line))
	 (name (subseq input-line 0 pos))
	 (rule (subseq input-line (1+ pos) (1- (length input-line)))))
    (list name (make-rule rule))))


(defun parse-parts (input-line)
  (let ((output '())
	(part-string (split (subseq input-line 1 (1- (length input-line)))
			    #\,)))
    (loop for part in part-string do
      (let ((symb (char part 0))
	    (value (parse-integer (subseq part 2))))
	(case symb
	  (#\x (setf (getf output :x) value))
	  (#\m (setf (getf output :m) value))
	  (#\a (setf (getf output :a) value))
	  (#\s (setf (getf output :s) value)))))
    (list :x (getf output :x)
	  :m (getf output :m)
	  :a (getf output :a)
	  :s (getf output :s))))


(defun read-input (input-fp)
  (let ((parts-list '())
	(reading-workflows 'T)
	(workflows (make-hash-table :size 10000 :test #'equal)))

    (with-open-file (stream input-fp)
      (loop for line = (read-line stream 'nil)
	    while line do
	      (cond  ((equal line "")
		      (setf reading-workflows 'nil))
		     (reading-workflows
		      (destructuring-bind (name rule) (parse-workflow line)
			(setf (gethash name workflows) rule)))
		     ('T (push (parse-parts line) parts-list)))))

    (list (reverse parts-list) workflows)))


(defun solve-single (part workflows next-rule)
  (let* ((func (gethash next-rule workflows))
	 (output (funcall func part)))
    (cond ((equal output "R")
	   #\R)
	  ((equal output "A")
	   #\A)
	  ('T (solve-single part workflows output)))))


(defun solve (parts-list workflows)
  (let ((accept-score 0))
    (loop for part in parts-list do
      (let ((result (solve-single part workflows "in")))
	(if (eql result #\A)
	    (setf accept-score (+ accept-score
				  (getf part :x)
				  (getf part :m)
				  (getf part :a)
				  (getf part :s))))))
    accept-score))

(time (let* ((input-fp "./input.txt")
	     (result 0))
	(destructuring-bind (parts-list workflows) (read-input input-fp)
	  (setf result (solve parts-list workflows )))
	(format t "Solution: ~s~%" result)))

;; 432427 is the right answer for part 1
;; run time is 0.000616 seconds of total run time (0.000599 user, 0.000017 system)

