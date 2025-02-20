(defun read-input-map (input-fp)
  (let ((mouse-map (make-hash-table :test 'equal))
	(i 0)
	(j 0))
    (with-open-file (stream input-fp)
      (loop for ch = (read-char stream 'nil) while ch do
	(if (eql ch #\Newline)
	    (progn (setf j 0)
		   (incf i))
	    (progn (setf (gethash (cons i j) mouse-map) ch)
		   (incf j)))
	(if (eql ch #\S)
	    (setf (gethash #\S mouse-map) (cons i (1- j))))))
    mouse-map))


(defun cons+ (a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))


(defun cons- (a b)
  (cons (- (car a) (car b)) (- (cdr a) (cdr b))))


(defun mth (idx list-in)
  (if (and (>= idx 0) (>= (1- (length list-in)) idx))
      (nth idx list-in)
      (nth (mod idx (length list-in)) list-in)))


(defun path-neighbours (pos map)
  (let ((val (gethash pos map))
	(output '())
	(n (cons+ pos '(-1 . 0)))
	(e (cons+ pos '(0 . 1)))
	(s (cons+ pos '(1 . 0)))
	(w (cons+ pos '(0 . -1))))
    (cond ((eql #\| val)
	   (if (find (gethash n map) "|F7S") (push n output))
	   (if (find (gethash s map) "|JLS") (push s output)))
	  ((eql #\F val)
	   (if (find (gethash e map) "-J7S") (push e output))
	   (if (find (gethash s map) "|JLS") (push s output)))
	  ((eql #\- val)
	   (if (find (gethash e map) "-J7S") (push e output))
	   (if (find (gethash w map) "-LFS") (push w output)))
	  ((eql #\7 val)
	   (if (find (gethash w map) "-LFS") (push w output))
	   (if (find (gethash s map) "|JLS") (push s output)))
	  ((eql #\J val)
	   (if (find (gethash n map) "|F7S") (push n output))
	   (if (find (gethash w map) "-LFS") (push w output)))
	  ((eql #\L val)
	   (if (find (gethash n map) "|F7S") (push n output))
	   (if (find (gethash e map) "-J7S") (push e output)))
	  (t
	   (if (find (gethash n map) "|F7S") (push n output))
	   (if (find (gethash e map) "-J7S") (push e output))
	   (if (find (gethash s map) "|JLS") (push s output))
	   (if (find (gethash w map) "-LFS") (push w output))))
    (if (= 2 (length output))
	output
	(error "Incorrect number of inputs"))))


(defun walk-map (input-hash-tab &optional (output '()))
  (if (= 0 (length output))
      (walk-map input-hash-tab (push (gethash #\S input-hash-tab) output))

      (let* ((curr (car output))
	     (prev (cadr output))
	     (neighbours (path-neighbours curr input-hash-tab)))
	(setf neighbours (remove prev neighbours :test 'equal))

	(if (and (equal curr (gethash #\S input-hash-tab)) (> (length output) 1))
	    output
	    (walk-map input-hash-tab (push (car neighbours) output))))))


(defun make-square (point)
  (let ((region '()))
    (loop for i from -1 to 1 do
      (loop for j from -1 to 1 do
	(push (cons+ point (cons i j)) region)))
    (remove point region :test 'equal)))


(defun right-left-hand-rules (point d0 d1)
  
  (let ((rights (make-hash-table :test 'equal :size 1000))
	(lefts (make-hash-table :test 'equal :size 1000)))
    (destructuring-bind (se s sw e w ne n nw) (make-square point)
      (cond ((equal d0 '(-1 . 0))
	     (cond ((equal d1 '(-1 . 0))
		    (setf (gethash sw lefts) 'T)
		    (setf (gethash w lefts) 'T)
		    (setf (gethash nw lefts) 'T)
		    (setf (gethash se rights) 'T)
		    (setf (gethash e rights) 'T)
		    (setf (gethash ne rights) 'T))
		   ((equal d1 '(0 . 1))
		    (setf (gethash w lefts) 'T)
		    (setf (gethash nw lefts) 'T)
		    (setf (gethash n lefts) 'T)
		    (setf (gethash se rights) 'T))
		   ((equal d1 '(0 . -1))
		    (setf (gethash sw lefts) 'T)
		    (setf (gethash e rights) 'T)
		    (setf (gethash ne rights) 'T)
		    (setf (gethash n rights) 'T))))
	    ((equal d0 '(0 . 1))
	     (cond ((equal d1 '(-1 . 0))
		    (setf (gethash nw lefts) 'T)
		    (setf (gethash s rights) 'T)
		    (setf (gethash se rights) 'T)
		    (setf (gethash e rights) 'T))
		   ((equal d1 '(0 . 1))
		    (setf (gethash nw lefts) 'T)
		    (setf (gethash n lefts) 'T)
		    (setf (gethash ne lefts) 'T)
		    (setf (gethash sw rights) 'T)
		    (setf (gethash s rights) 'T)
		    (setf (gethash se rights) 'T))
		   ((equal d1 '(1 . 0))
		    (setf (gethash n lefts) 'T)
		    (setf (gethash ne lefts) 'T)
		    (setf (gethash e lefts) 'T)
		    (setf (gethash sw rights) 'T))))
	    ((equal d0 '(1 . 0))
	     (cond ((equal d1 '(0 . 1))
		    (setf (gethash ne lefts) 'T)
		    (setf (gethash w rights) 'T)
		    (setf (gethash sw rights) 'T)
		    (setf (gethash w rights) 'T))
		   ((equal d1 '(1 . 0))
		    (setf (gethash ne lefts) 'T)
		    (setf (gethash e lefts) 'T)
		    (setf (gethash se lefts) 'T)
		    (setf (gethash nw rights) 'T)
		    (setf (gethash w rights) 'T)
		    (setf (gethash sw rights) 'T))
		   ((equal d1 '(0 . -1))
		    (setf (gethash e lefts) 'T)
		    (setf (gethash se lefts) 'T)
		    (setf (gethash s lefts) 'T)
		    (setf (gethash nw rights) 'T))))
	    ((equal d0 '(0 . -1))
	     (cond ((equal d1 '(-1 . 0))
		    (setf (gethash ne rights) 'T)
		    (setf (gethash s lefts) 'T)
		    (setf (gethash sw lefts) 'T)
		    (setf (gethash w lefts) 'T))
		   ((equal d1 '(1 . 0))
		    (setf (gethash n rights) 'T)
		    (setf (gethash nw rights) 'T)
		    (setf (gethash w rights) 'T)
		    (setf (gethash se lefts) 'T))
		   ((equal d1 '(0 . -1))
		    (setf (gethash ne rights) 'T)
		    (setf (gethash n rights) 'T)
		    (setf (gethash nw rights) 'T)
		    (setf (gethash se lefts) 'T)
		    (setf (gethash s lefts) 'T)
		    (setf (gethash sw lefts) 'T))))))
    (return-from right-left-hand-rules (list lefts rights))))



(defun starts (map route)
  (let ((deltas0 (loop for idx from 0 to (length route)
		       collect (cons-  (mth idx route) (mth (1- idx) route))))
	(deltas1 (loop for idx from 0 to (length route)
		       collect (cons- (mth (1+ idx) route) (mth idx route))))
	(rights (make-hash-table :test 'equal :size 1000))
	(lefts (make-hash-table :test 'equal :size 1000)))
    
    (loop for idx from 0 for point in route do
      (let* ((d0 (nth idx deltas0))
	     (d1 (nth idx deltas1))
	     (local-left-right (right-left-hand-rules point d0 d1)))
	(destructuring-bind (local-left local-right) local-left-right
	  (maphash (lambda (k v) (declare (ignore v))
		     (if (and (gethash k map) (not (member k route :test 'equal)))
			 (setf (gethash k lefts) 'T)))
		   local-left)
	  (maphash (lambda (k v) (declare (ignore v))
		     (if (and (gethash k map) (not (member k route :test 'equal)))
			 (setf (gethash k rights) 'T)))
		   local-right))))
    (cons lefts rights)))

(defun nesw-neighbours (map point burnt)
  (let ((neighbours '())
	(neighbour))
    (loop for delta in '((-1 . 0) (0 . 1) (1 . 0) (0 . -1)) do
      (setf neighbour (cons+ point delta))
      (if (and (gethash neighbour map) (null (gethash neighbour burnt)))
	  (push neighbour neighbours)))
    neighbours))


(defun light-fires (map route)
  (let* ((active (car (starts map route)))
	 (burnt (make-hash-table :test 'equal :size 5000)))

    (format t "Number starts: ~s~%" (hash-table-count active))
    (loop for toast in route do (setf (gethash toast burnt) 'T))

    (loop while (> (hash-table-count active) 0) do
      (loop for k being the hash-key in active do
	(setf (gethash k burnt) 'T)
	(loop for ngh in (nesw-neighbours map k burnt) do
	  (setf (gethash ngh active) 'T))
	(remhash k active)))
    burnt))


(let* ((input-fp "./input.txt")
       (map (read-input-map input-fp))
       (route (butlast (walk-map map)))
       (burnt-lhr (light-fires map route))
       (lhr-soln (- (hash-table-count burnt-lhr)
		    (length (remove-duplicates route :test #'equal)))))
  (format t "Nesting spaces: ~s~%" lhr-soln))



;; 7086 is the right answer for part 1
;; 5117 is too high for part 2
;; 5116 is too high
;; 313 is too low for part 2
;; 317 is the correct answer for part 2
