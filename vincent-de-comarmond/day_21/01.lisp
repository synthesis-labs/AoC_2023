(defun read-input (input-fp)
  (let ((lines '())
	(start-idx '()))
    (with-open-file (stream input-fp)
      (setf lines (loop for row-idx from 0
			for line = (read-line stream 'nil)
			while line
			when (position #\S line)
			  do (push (position #\S line) start-idx)
			     (push row-idx start-idx)
			end
			collect line)))

    (let ((grid (make-array (list (length lines) (length (car lines)))
			    :element-type 'character :initial-element #\.)))
      
      (loop for row-idx from 0 for row in lines do
	(loop for col-idx from 0 for char_ across row do
	  (setf (aref grid row-idx col-idx) char_)))
      (list grid start-idx))))


(defun fast-march-n-steps (grid steps active)
  ;; (format t "Steps: ~s~%" steps)
  (let ((sizes (array-dimensions grid))
	(next-gen '()))

    (loop for element in active do
      (loop for neighbour in '((-1 0) (1 0) (0 -1) (0 1)) do
	(let ((i (+ (car element) (car neighbour)))
	      (j (+ (cadr element) (cadr neighbour))))
	  (if (and (<= 0 i)
		   (<= 0 j)
		   (< i (car sizes))
		   (< j (cadr sizes))
		   (find (aref grid i j) '(#\S #\.)))
	      (setf next-gen (adjoin (list i j) next-gen :test #'equal))))))
    (if (= steps 1)
	next-gen
	(fast-march-n-steps grid (1- steps) next-gen))))

(time (let ((input-fp "./input.txt")
	    (steps 64)
	    (possible-end-positions '()))
	(destructuring-bind (grid start-position) (read-input input-fp)
	  (setf possible-end-positions (fast-march-n-steps grid steps (list start-position))))
	(format t "Possible end positions: ~s~%" possible-end-positions)
	(format t "Possible end positions: ~s~%" (length possible-end-positions))))


;; Part 1 run time 0.391
;; 3724 is the right answer for part 1
