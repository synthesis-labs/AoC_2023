(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))

(defun parse-line (line)
  (destructuring-bind (start end) (split-str line #\@)
    (destructuring-bind ((x y z) (dx dy dz))
	(mapcar (lambda (x) (mapcar 'parse-integer (split-str x #\,))) (list start end))
      (vector x y z dx dy dz))))

(defun read-input (input-fp)
  (with-open-file (stream input-fp)
    (loop for idx from 0
	  for line = (read-line stream 'nil)  while line
	  collect (parse-line line))))

(defun x (vec1 vec2)
  (let ((vec3 (make-array '(3))))
    (setf (aref vec3 0)
	  (- (* (aref vec1 1) (aref vec2 2))
	     (* (aref vec1 2) (aref vec2 1))))
    (setf (aref vec3 1)
	  (- (* (aref vec1 2) (aref vec2 0))
	     (* (aref vec1 0) (aref vec2 2))))
    (setf (aref vec3 2)
	  (- (* (aref vec1 0) (aref vec2 1))
	     (* (aref vec1 1) (aref vec2 0))))
    vec3))

(defun det (matrix)
  (let ((dim (array-dimension matrix 0)))
    (if (equal '(2 2) (array-dimensions matrix))
	(- (* (aref matrix 0 0) (aref matrix 1 1))
	   (* (aref matrix 1 0) (aref matrix 0 1)))
	(let ((dummy (make-array (list (1- dim) (1- dim))))
	      (total 0)
	      multiplier)
	  (loop for k from 0 below dim do
	    (setf multiplier (if (= 0 (mod k 2))
				 (aref matrix k 0)
				 (- (aref matrix k 0))))

	    (loop for i from 0 below dim do
	      (loop for j from 1 below dim do
		(cond ((= i k))
		      ((< i k)
		       (setf (aref dummy i (1- j)) (aref matrix i j)))
		      ('t
		       (setf (aref dummy (1- i) (1- j)) (aref matrix i j)))
		  )))
	    (setf total (+ total (* multiplier (det dummy)))))
	  total))))

(defun transpose (matrix)
  (let ((output (make-array (array-dimensions matrix) :element-type (array-element-type matrix))))
    (loop for i from 0 below (array-dimension matrix 0) do
      (loop for j from 0 below (array-dimension matrix 1) do
	(setf (aref output i j) (aref matrix j i))))
    output))

(defun adjoint (matrix)
  (let* ((dim (array-dimension matrix 0))
	 (dummy (make-array (list (1- dim) (1- dim)) :element-type (array-element-type matrix)))
	 (cofactor-matrix (make-array (list dim dim) :element-type (array-element-type matrix))))

    (loop for i from 0 below dim do
      (loop for j from 0 below dim do

	(loop for ii from 0 below dim do
	  (loop for jj from 0 below dim do
	    (cond ((or (= i ii) (= j jj)))
		  ((and (< ii i) (< jj j))
		   (setf (aref dummy ii jj) (aref matrix ii jj)))
		  ((< ii i)
		   (setf (aref dummy ii (1- jj)) (aref matrix ii jj)))
		  ((< jj j)
		   (setf (aref dummy (1- ii) jj) (aref matrix ii jj)))
		  ('T
		   (setf (aref dummy (1- ii) (1- jj)) (aref matrix ii jj))))))
	(let ((cofactor (if (= 0 (mod (+ i j) 2))
			    (det dummy)
			    (- (det dummy)))))
	    (setf (aref cofactor-matrix i j) cofactor))))
    (transpose cofactor-matrix)))

(defun invert (matrix)
  (let ((dim (array-dimension matrix 0))
	(determinant (det matrix))
	(adjoint_ (adjoint matrix))
	(output (make-array (array-dimensions matrix) :element-type (array-element-type matrix))))
    (loop for i from 0 below dim do
      (loop for j from 0 below dim do
	    (setf (aref output i j) (/ (aref adjoint_ i j) determinant))))
    output))


(defun mmult (matrix1 matrix2)
  (let* ((dims1 (array-dimensions matrix1))
	 (dims2 (array-dimensions matrix2))
	 (output (make-array (list (car dims1) (cadr dims2)))))

    (loop for i from 0 below (car dims1) do
      (loop for j from 0 below (cadr dims2) do
	(let ((total 0))
	  (loop for k from 0 below (cadr dims1) do
	    (setf total (+ total (* (aref matrix1 i k)
				    (aref matrix2 k j)))))
	  (setf (aref output i j) total))))
    output))


(defun setup-matrix-equations (trajectories)
  (let* ((h1 (first trajectories))
	 (h2 (second trajectories))
	 (h3 (third trajectories))
	 (pdiff1 (map 'vector '- (subseq h1 0 3) (subseq h2 0 3)))
	 (vdiff1 (map 'vector '- (subseq h2 3) (subseq h1 3)))
	 (rhs1 (map 'vector '-
		    (x (subseq h2 0 3) (subseq h2 3))
		    (x (subseq h1 0 3) (subseq h1 3))))
	 (pdiff2 (map 'vector '- (subseq h1 0 3) (subseq h3 0 3)))
	 (vdiff2 (map 'vector '- (subseq h3 3) (subseq h1 3)))
	 (rhs2 (map 'vector '-
		    (x (subseq h3 0 3) (subseq h3 3))
		    (x (subseq h1 0 3) (subseq h1 3))))
	 (matrix (make-array '(6 6) :initial-element 0))
	 rhs)

    	(loop for offset in '(0 3) do
	  (let ((vdiff (if (= offset 0) vdiff1 vdiff2))
		(pdiff (if (= offset 0) pdiff1 pdiff2)))

	    (setf (aref matrix (+ 0 offset) 1)    (aref vdiff 2))
	    (setf (aref matrix (+ 0 offset) 2) (- (aref vdiff 1)))
	    (setf (aref matrix (+ 0 offset) 4)    (aref pdiff 2))
	    (setf (aref matrix (+ 0 offset) 5) (- (aref pdiff 1)))

	    (setf (aref matrix (+ 1 offset) 0) (- (aref vdiff 2)))
	    (setf (aref matrix (+ 1 offset) 2)    (aref vdiff 0))
	    (setf (aref matrix (+ 1 offset) 3) (- (aref pdiff 2)))
	    (setf (aref matrix (+ 1 offset) 5)    (aref pdiff 0))

	    (setf (aref matrix (+ 2 offset) 0)    (aref vdiff 1))
	    (setf (aref matrix (+ 2 offset) 1) (- (aref vdiff 0)))
	    (setf (aref matrix (+ 2 offset) 3)    (aref pdiff 1))
	    (setf (aref matrix (+ 2 offset) 4) (- (aref pdiff 0)))))

	(setf rhs (make-array '(6 1) :initial-contents
			      (loop for j in (list rhs1 rhs2)
				    nconcing (loop for i across j collect (list i)))))
    (list matrix rhs)))


(time (let* ((input-fp "./input.txt")
	     (trajectories (read-input input-fp))
	     x-inv
	     solution)

	(destructuring-bind (x y) (setup-matrix-equations trajectories)
	  (setf x-inv (invert x))
	  (setf solution (mmult x-inv y))
	  (format t "Inversion test: ~s~%" (mmult x-inv x)))

	(format t "Initial conditions: ~s~%" solution)
	(let* ((x0 (aref solution 0 0))
	       (y0 (aref solution 1 0))
	       (z0 (aref solution 2 0))
	       (position-score (+ x0 y0 z0)))
	  (format t "Position score: ~s~%" position-score))))

;; 27732 is the right answer for part 1
;; run time ~ 0.12 seconds

;; 641619849766168 is the right answer for part 2
;; Solution is instantaneous
;; but to be fair I wouldn't have found it without the forums
