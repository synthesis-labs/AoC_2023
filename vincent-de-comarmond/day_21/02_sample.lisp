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


(defun get-neighbours-mod (grid i j)
  (let ((sizes (array-dimensions grid))
	(neighbours '()))
    (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1)) do
      (let* ((i (+ i di))
	     (mod-i (mod i (car sizes)))
	     (j (+ j dj))
	     (mod-j (mod j (cadr sizes))))
	(if (find (aref grid mod-i mod-j) '(#\. #\S))
	    (push (list i j) neighbours))))
    neighbours))


(defun fast-march (grid active max-steps &optional (steps 0) (burnt '()))
  (if (= 0 (mod steps 10)) (format t "Steps: ~a~%" steps))
  (let ((next-gen '()))
    (loop for (i j) in active do
      (push (list i j steps) burnt)

      (when (< steps max-steps)
	(loop for (ni nj) in (get-neighbours-mod grid i j) do
	  (if (not (find-if (lambda (x) (and (= (car x) ni)
					     (= (cadr x) nj)))
			    burnt))
	      (setf next-gen (adjoin (list ni nj) next-gen :test #'equal))))))

    (if (null next-gen)
	burnt
	(fast-march grid next-gen max-steps (1+ steps) burnt))))


(defun solve-quadratic (points)
  ;; Guess that b is 0 (the problem doesn't make sense otherwise)
  (let* ((a 'nil)
	 (b 'nil)
	 (c 'nil)
	 (points-of-int (subseq points (- (length points) 3))))
    (let ((x1 (car (first points-of-int)))
	  (y1 (cadr (first points-of-int)))
	  (x2 (car (second points-of-int)))
	  (y2 (cadr (second points-of-int)))
	  (x3 (car (third points-of-int)))
	  (y3 (cadr (third points-of-int))))

      (setf a (- (/ (- y3 y2)
		    (* (- x3 x2) (- x3 x1)))
		 (/ (- y2 y1)
		    (* (- x2 x1) (- x3 x1)))))
      (setf b (- (/ (- y3 y2)
		    (- x3 x2))
		 (* a (+ x3 x2))))
      (setf c (- y1 (* a x1 x1) (* b x1)))
      (list :a a :b b :c c))))


(defun prediction-engine (parameter-map)
  #'(lambda (x)
      (destructuring-bind (&key a b c) (gethash (mod x 11) parameter-map)
	(+ (* a x x) (* b x) c))))

;; The gnuplot of this shows a clearly quadratic pattern
;; The minimum should surely be at 0
;; There seems to be a difference between even and odd step numbers

;; After some prodding it seems like relationship is practically 2*x*x/3 with odd-even jitter
;; odd-even jitter is too much to ignore ... try for odds only

;; Cool ... so with evens only the pattern is nearly perfect, with slight visible periodicity
;; Okay so 0.67 works better that 2/3.periods are also clearly visible on the graphs. first 20 steps at least is garbage

;; visual inspection shows periodicity of 11
;; mathematical inspection confirms periodicity of 11. All second differences are 162
;; after the first few terms


(time (let* ((input-fp "./sample.txt")
	     (step-limit 100)
	     (grid-start (read-input input-fp))
	     (grid (car grid-start))
	     (start-pos (cadr grid-start))
	     (all-points (fast-march grid (list start-pos) step-limit))
	     (function-inference '())
	     (mod-11-hash (make-hash-table))
	     (quadratic-parameters (make-hash-table))
	     (make-predictions 'nil))

	(setf function-inference
	      (loop for t_ from 0 to step-limit
		    collect (list t_ (length
				      (if (evenp t_)
					  (remove-if-not (lambda (x) (and (evenp (caddr x))
									  (<= (caddr x) t_)))
							 all-points)
					  (remove-if-not (lambda (x) (and (oddp (caddr x))
									  (<= (caddr x) t_)))
							 all-points))))))

	
	(with-open-file (fl-out "./function-inference.dat"
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	  (loop for (steps num) in function-inference do
	    (format fl-out "~a ~a~%" steps num)))


	(loop for period from 0 below 11 do
	  (setf (gethash period mod-11-hash )
		(loop for (i j) in function-inference
		      if (= period (mod i 11)) collect (list i j))))


	(maphash (lambda (k v)
		   (setf (gethash k quadratic-parameters) (solve-quadratic v)))
		 mod-11-hash)

	(setf make-predictions (prediction-engine quadratic-parameters))
	(loop for (steps result) in '((6 16) (10 50) (50 1594) (100 6536) (500 167004)
				      (1000 668697) (5000 16733044))
	      do (let ((prediction (if (< steps (length function-inference))
				       (cadr (nth steps function-inference))
				       (funcall make-predictions steps))))
		   (format t "Steps: ~s~%" steps)
		   (format t "~cPrediction: ~s~%" #\tab prediction)
		   (format t "~c   Correct: ~s~%" #\tab result)))))
