(defun input-line-to-list (line &optional (accum '()))
  (let ((chop (position #\Space line)))
    (if (null chop)
	(nreverse (push (parse-integer line) accum))
	(input-line-to-list (subseq line (1+ chop))
			    (push (parse-integer
				   (subseq line 0 chop))
				  accum)))))


(defun walk-down (line &optional (accum))
  (setf accum (if (null accum)
		  (list line)
		  accum))
  (let ((l (car accum)))
    (if (every (lambda (x) (= x 0)) l)
	accum
	(progn (push (loop for idx from 1 to (1- (length l))
			   collect (- (nth idx l) (nth (1- idx) l)))
		     accum)
	       (walk-down l accum)))))


(defun  walk-up (delta-list &optional (prev 0))
  (if (= (length delta-list) 1)
      (- (car (car delta-list))  prev)
      (walk-up (rest delta-list) (-  (car (car delta-list)) prev))))


(let ((input-fp "./input.txt")
      (total 0))
  (with-open-file (stream input-fp)
    (loop for line = (read-line stream 'nil) while line
	  do (setf total (+ total (walk-up
				   (walk-down
				    (input-line-to-list line)))))))
  (format t "OASIS REPORT: ~s~%" total))

;; 1884768153 is the right answer for part 1
;; 1031 is the right answer for part 2
