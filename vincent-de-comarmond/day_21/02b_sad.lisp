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

    (loop while (> (length active) 0)  for idx from 0 do
      ;; (format t "Fast march invocation: ~s~%" idx)
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


(defun get-parities (burnt-grid)
  (let ((steps (loop for v being the hash-values of burnt-grid collect v)))
    (list :even-count (length (remove-if-not 'evenp steps))
	  :odd-count (length (remove-if-not 'oddp steps))
	  :max-steps (apply 'max steps))))


(defun make-reference-grids (hash-grid ni nj)
  (let ((reference-grids (make-hash-table :size 10 :test #'equal)))

    (loop for i in (list (- ni) 0 ni) do
      (loop for j in (list (- nj) 0 nj) do
	(setf (gethash (cons i j) reference-grids)
	      (fast-march hash-grid (list (list i j 0)) :expanded-grid-p 'nil :max-steps 'nil))))

    reference-grids))


(defun solve-small (grid max-steps &optional (start-point '(0 0 0)))
  (let* ((burnt (fast-march grid (list start-point)
			    :expanded-grid-p 'T
			    :max-steps max-steps))
	 (burnt-points (loop for v being the hash-values of burnt
			     collect v)))
    (if (oddp max-steps)
	(length (remove-if-not 'oddp burnt-points))
	(length (remove-if-not 'evenp burnt-points)))))


(defun solve-big (hash-grid max-steps)
  (let* ((keys (loop for k being the hash-keys of hash-grid collect k))
	 (ni (apply 'max (mapcar 'car keys)))
	 (nj (apply 'max (mapcar 'cdr keys)))
	 (grid-size (+ ni nj 1))
	 (hyper-dist (1+ (floor max-steps grid-size))))
    

    (let ((parities (make-hash-table :test #'equal :size 10))
	  (parity-func (if (evenp max-steps) 'evenp 'oddp))
	  (ref-grids (make-reference-grids hash-grid ni nj))
	  (diag-dist (- (* hyper-dist grid-size) ni nj))
	  (h-dist (- (* hyper-dist grid-size) nj))
	  (v-dist (- (* hyper-dist grid-size) ni))
	  (left-overs '())
	  (total 0)
	  counters
	  grid_
	  dist_)

      (maphash (lambda (k v) (setf (gethash k parities) (get-parities v))) ref-grids)
      (setf counters (gethash '(0 . 0) parities))
      ;; (format t "Hyper distance: ~s~%" hyper-dist)

      
      (loop for i from (- hyper-dist) to hyper-dist do
	(loop for j from (- (abs i) hyper-dist) to (- hyper-dist (abs i)) do

	  (let ((complete (<= (+ (* (+ (abs i) (abs j)) grid-size) (1- grid-size)) max-steps)))
	    ;; (format t "Visiting: ~s, ~s~%" i j)
	    ;; (format t "Number: ~s~%" (+ (* (+ (abs i) (abs j)) grid-size) (1- grid-size)))
	    ;; (format t "Complete: ~s~%~%" complete)

	  (if (not complete)
	      (progn (push (cons i j) left-overs)
		     (format t "Number extras: ~s~%" (length left-overs)))
	      (progn (format t "Hyper grid visit: ~s, ~s~%" i j)
		     (if (evenp (+ i j))
			 (if (evenp max-steps)
			     (setf total (+ total (getf counters :even-count)))
			     (setf total (+ total (getf counters :odd-count))))
			 (if (oddp max-steps)
			     (setf total (+ total (getf counters :even-count)))
			     (setf total (+ total (getf counters :odd-count)))))))
	    
	    
	    )


	  


	      ))

      (format t "Partial count: ~s~%" total)

      (loop for (i . j) in left-overs do
	(cond ((< i 0) (cond ((< j 0)
			      (setf dist_ diag-dist)
			      (setf grid_ (gethash (cons ni nj) ref-grids)))
			     ((= j 0)
			      (setf dist_ v-dist)
			      (setf grid_ (gethash (cons ni 0) ref-grids)))
			     ((> j 0)
			      (setf dist_ diag-dist)
			      (setf grid_ (gethash (cons ni (- nj)) ref-grids)))))
	      ((= i 0) (cond ((< j 0)
			      (setf dist_ h-dist)
			      (setf grid_ (gethash (cons 0 nj) ref-grids)))
			     ((= j 0)
			      (setf dist_ 0)
			      (setf grid_ (gethash '(0 . 0) ref-grids)))
			     ((> j 0)
			      (setf dist_ h-dist)
			      (setf grid_ (gethash (cons 0 (- nj)) ref-grids)))))
	      ((> i 0) (cond ((< j 0)
			      (setf dist_ diag-dist)
			      (setf grid_ (gethash (cons (- ni) nj) ref-grids)))
			     ((= j 0)
			      (setf dist_ v-dist)
			      (setf grid_ (gethash (cons (- ni) 0) ref-grids)))
			     ((> j 0)
			      (setf dist_ diag-dist)
			      (setf grid_ (gethash (cons (- ni) (- nj)) ref-grids))))))

	;; (format t "Visiting space: ~s, ~s~%" i j)
	;; (format t "Offset distance: ~s~%" dist_)
	;; (format t "Before: ~s~%" total)
	
	(loop for v being the hash-values of grid_ do
	  (if (and (<= (+ dist_ v) max-steps)
		   (funcall parity-func (+ dist_ v)))
	      (incf total)))

	;; (format t "After: ~s~%~%" total)

	    )

      

      total)))


(time (let* ((input-fp "./input.txt")
	     (max-steps 75)
	     (hash-grid (read-input input-fp))
	     (small-soln (solve-small hash-grid max-steps))
	     (big-soln (solve-big hash-grid max-steps))
	     )

	(format t "solve small: ~s~%" small-soln)
	(format t "solve large: ~s~%" big-soln)
	(format t "big - small soln: ~s~%" (- big-soln small-soln))
	))
