(defun read-input-file (input-fp)
  (let ((galaxies '())
	(i 0)
	(j 0))
    (with-open-file (stream input-fp)
      (loop for ch = (read-char stream 'nil)
	    while ch do
	      (cond ((equal ch #\Newline)
		     (incf i)
		     (setf j -1))
		    ((equal ch #\#)
		     (push (cons i j) galaxies)))
	      (incf j)))
    (sort galaxies (lambda (a b)
		     (if (< (car a) (car b)) 'T
			 (if (and (= (car a) (car b))
				  (< (cdr a) (cdr b)))
			     'T))))))

(defun get-distances (galaxies expansion-factor)
  (let ((max-galaxy (1- (length galaxies)))
	(non-empty-rows (remove-duplicates (mapcar 'car galaxies) :test 'equal))
	(non-empty-cols (remove-duplicates (mapcar 'cdr galaxies) :test 'equal))
	(distances '()))

    (loop for a from 0 to max-galaxy do
      (loop for b from (1+ a) to max-galaxy do
	(let ((alpha (nth a galaxies))
	      (beta (nth b galaxies))
	      (dy 0)
	      (dx 0))
	  (loop for i from (1+ (min (car alpha) (car beta))) to (max (car alpha) (car beta))
		do (if (member i non-empty-rows :test 'equal)
		       (incf dy)
		       (setf dy (+ dy expansion-factor))))
	  (loop for j from (1+ (min (cdr alpha) (cdr beta))) to (max (cdr alpha) (cdr beta))
		do (if (member j non-empty-cols :test 'equal)
		       (incf dx)
		       (setf dx (+ dx expansion-factor))))
	  (push (+ dy dx) distances))))
    distances))

(let* ((galaxies (read-input-file "./input.txt"))
       (distances (get-distances galaxies 1000000)))
  (format t "Galaxy distance sum: ~s~%" (apply '+ distances)))

;; 10231178 is the right answer for part 1
;; 622120986954 is the correct answer for part 2
