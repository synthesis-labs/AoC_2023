(defun read-input (input-fp)
  (let ((lines '()))
    (with-open-file (stream input-fp)
      (setf lines (loop for line = (read-line stream 'nil) while line
			collect line)))

    (let ((hash-grid (make-hash-table :test #'equal))
	  (half-n-rows (/ (1- (length lines)) 2))
	  (half-n-cols (/ (1- (length (car lines))) 2)))
      (loop for i from 0 for line in lines do
	(loop for j from 0 for char_ across line do
	  (setf (gethash (cons (- i half-n-rows) (- j half-n-cols)) hash-grid)
		(if (eql #\S char_) #\. char_))))
      hash-grid)))


(defun neighbour-getter (hash-grid &key (expanded-grid-p 'nil))
  (let* ((keys (loop for k being the hash-keys of hash-grid collect k))
	 (min-i (apply 'min (mapcar 'car keys)))
	 (dim-i (- (apply 'max (mapcar 'car keys)) min-i))
	 (min-j (apply 'min (mapcar 'cdr keys)))
	 (dim-j (- (apply 'max (mapcar 'cdr keys)) min-j)))
    #'(lambda (i j)
	(let ((neighbours '()))
	  (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1)) do
	    (destructuring-bind (ni nj) (list (+ i di) (+ j dj))
	      (if (null expanded-grid-p)
		  (if (eql (gethash (cons ni nj) hash-grid) #\.)
		      (push (cons ni nj) neighbours))
		  (let ((mod-i (+ (mod (- ni min-i) dim-i) min-i))
			(mod-j (+ (mod (- nj min-j) dim-j) min-j)))
		    (if (eql (gethash (cons mod-i mod-j) hash-grid) #\.)
			(push (cons ni nj) neighbours))))))
	  neighbours))))


(defun fast-march (hash-grid active &key expanded-grid-p max-steps)
  (let ((neighbour-func (neighbour-getter hash-grid :expanded-grid-p expanded-grid-p))
	(next-gen '())
	(neighbours '())
	(burnt (make-hash-table :test #'equalp
				:size (* 2 (hash-table-count hash-grid)))))

    (loop while (> (length active) 0) for idx from 0 do
      (if (= 0 (mod idx 50))
	  (format t "MAX STEP: ~s~%" (apply 'max (mapcar 'caddr active))))
      (loop for (i j steps) in active do
	(setf (gethash (cons i j) burnt) steps)
	(setf neighbours (funcall neighbour-func i j))
	(if (or (null max-steps)
		(< steps max-steps))
	    (loop for (ni . nj) in neighbours
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Visual inspection shows peridicity of 130			 ;;
;; (with-open-file (fl-out "./02.dat" :direction :output		 ;;
;; 				   :if-exists :supersede		 ;;
;; 				   :if-does-not-exist :create)		 ;;
;;   (loop for idx from 0 for num in data do				 ;;
;;     (format fl-out "~a ~a~%" idx num)))				 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(time (let* ((input-fp "./input.txt")
	     (max-steps 2000) ;; definitely far more samples than needed
	     (predict-steps 26501365)
	     (hash-grid (read-input input-fp))
	     (burnt (fast-march hash-grid '((0 0 0)) :expanded-grid-p 'T :max-steps max-steps))
	     (data (loop for idx from 0 to max-steps
			 collect
			 (length (loop for v being the hash-values of burnt
				       if (and (<= v idx)
					       (or (and (evenp idx) (evenp v))
						   (and (oddp idx) (oddp v))))
					 collect v))))
	     (periodicity-grid (make-hash-table :size 150))
	     (quadratic-params (make-hash-table :size 150))
	     predict)

	(loop for idx from 0 below 130 do
	  (setf (gethash idx periodicity-grid)
		(loop for i from 0 for dist in data
		      if (= idx (mod i 130)) collect (list i dist))))

	(maphash (lambda (k v)
		   (setf (gethash k quadratic-params) (solve-quadratic v)))
		 periodicity-grid)
	(setf predict (prediction-engine quadratic-params))

	;;;; Test that we're not wrong
	(loop repeat 10 do
	  (let* ((sample (+ 1500 (random 350)))
		 (brute-force-soln (nth sample data))
		 (predicted-soln (funcall predict sample)))
	    (format t "Test value: ~s~%" sample)
	    (format t "Brute-forced soln : ~s~%" brute-force-soln)
	    (format t "Predicted solution: ~s~%" predicted-soln)
	    (format t "Difference: ~s~%" (- brute-force-soln predicted-soln))))

	(format t "Solution is: ~s~%" (funcall predict predict-steps))))

;;;; 617212571644516 is too low
;;;; 620952791880287 is too high

