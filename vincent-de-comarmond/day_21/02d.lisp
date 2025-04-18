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


(defun fast-march (grid active &key max-steps)
  (let ((burnt (make-hash-table :test #'equalp
				:size (apply '* (array-dimensions grid))))
	next-gen
	neighbours)

    (loop while (> (length active) 0) for idx from 0 do
      (if (= 0 (mod idx 50))
	  (format t "MAX STEP: ~s~%" (apply 'max (mapcar 'caddr active))))
      (loop for (i j steps) in active do
	(setf (gethash (cons i j) burnt) steps)
	(setf neighbours (get-neighbours-mod grid i j))
	(if (or (null max-steps)
		(< steps max-steps))
	    (loop for (ni nj) in neighbours
		  if (null (gethash (cons ni nj) burnt)) do
		    (setf next-gen (adjoin (list ni nj (1+ steps)) next-gen :test #'equal)))))
      (setf active next-gen)
      (setf next-gen '()))
    burnt))


(defun solve-quadratic (points)
  (destructuring-bind ((x1 y1) (x2 y2) (x3 y3))
      (subseq points (- (length points) 3))
    (let* ((a (- (/ (- y3 y2) (* (- x3 x2) (- x3 x1)))
		 (/ (- y2 y1) (* (- x2 x1) (- x3 x1)))))
	   (b (- (/ (- y3 y2) (- x3 x2))
		 (* a (+ x3 x2))))
	   (c (- y1 (* a x1 x1) (* b x1))))

      (list :a a :b b :c c))))


(defun prediction-engine (quadratic-parameter-map &optional (periodicity 130))
  #'(lambda (x)
      (destructuring-bind (&key a b c) (gethash (mod x periodicity)
						quadratic-parameter-map)
	(+ (* a x x) (* b x) c))))


(time (let* ((input-fp "./input.txt")
	     (step-limit 2000) ;; definitely far more samples than needed
	     (predict-steps 26501365)
	     (quadratic-params (make-hash-table :size 20))
	     (periodicity 262)
	     prediction
	     sample-set
	     data)

	(destructuring-bind (grid start) (read-input input-fp)
	  (setf sample-set (fast-march grid (list (list (car start) (cadr start) 0))
				       :max-steps step-limit)))

	(setf data (loop for t_ from 0 to step-limit
			 collect
			 (list t_ (length (loop for v being the hash-values of sample-set
						if (and (<= v t_)
							(or (and (evenp t_) (evenp v))
							    (and (oddp t_) (oddp v))))
						  collect v)))))

	;; Periodicity looks like 262
	;; (with-open-file (fl-out "./02.dat" :direction :output
	;; 				   :if-exists :supersede
	;; 				   :if-does-not-exist :create)
	;;   (loop for (steps num) in data do
	;;     (format fl-out "~a ~a~%" steps num)))

	(loop for idx from 0 below periodicity do
	  (let* ((modulo-nums (loop for datum in data if (= idx (mod (car datum) periodicity))
				    collect datum))
		 (values (mapcar 'cadr modulo-nums))
		 (first-diff (mapcar '- (rest values) values))
		 (second-diff (mapcar '- (rest first-diff) first-diff)))

	    (format t "Second difference: ~s~%" second-diff)
	    (setf (gethash idx quadratic-params)
		  (solve-quadratic modulo-nums))))

	(setf prediction (prediction-engine quadratic-params periodicity))
	(format t "Solution: ~s~%" (funcall prediction predict-steps))))

;; 620348631910321 .... is the correct answer .... but really
;; f-king hell ... did they have to make it so tough
