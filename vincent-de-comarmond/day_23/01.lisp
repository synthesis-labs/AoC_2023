(defun read-input (input-fp)
  (let ((map_ (make-hash-table :size (* 150 150) :test #'equal))
	(neighbours (make-hash-table :size (* 150 150) :test #'equal)))

    (with-open-file (stream input-fp)
      (loop for i from 0
	    for line = (read-line stream 'nil)
	    while line do
	      (loop for j from 0
		    for char_ across line do
		      (if (not (eql char_ #\#))
			  (setf (gethash (cons i j) map_) char_)))))

    (maphash
     (lambda (k v)
       (destructuring-bind (i . j) k
	 (setf (gethash k neighbours) '())
	 (case v
	   (#\^ (push (cons (1- i) j) (gethash k neighbours)))
	   (#\> (push (cons i (1+ j)) (gethash k neighbours)))
	   (#\< (push (cons i (1- j)) (gethash k neighbours)))
	   (#\v (push (cons (1+ i) j) (gethash k neighbours)))
	   (#\.
	    (push (cons (1- i) j) (gethash k neighbours))
	    (push (cons i (1+ j)) (gethash k neighbours))
	    (push (cons i (1- j)) (gethash k neighbours))
	    (push (cons (1+ i) j) (gethash k neighbours))))))
     map_)

    (list map_ neighbours)))


(defun dfs (hash-map neighbour-map)
  (let* ((points (loop for k being the hash-keys of hash-map collect k))
	 (rows (mapcar 'car points))
	 (cols (mapcar 'cdr points))
	 (start (cons (apply 'min rows) (apply 'min cols)))
	 (end (cons (apply 'max rows) (apply 'max cols)))
	 (paths (list (list start)))
	 (longest 0)
	 longest-path)

    (loop while (< 0 (length paths)) do
      (let* ((path (pop paths))
	     (head (car path))
	     (neighbours (gethash head neighbour-map)))

	(cond ((equal head end)
	       (when (> (1- (length path)) longest)
		 (format t "Found longer path of length: ~s~%" (1- (length path)))
		 (setf longest (max longest (1- (length path))))
		 (setf longest-path path)))
	      ((= 1 (length neighbours))
	       (when (not (member (car neighbours) path :test #'equal))
		 (push (car neighbours) path)
		 (push path paths)))
	      ('T (loop for neighbour in neighbours
			if (not (member neighbour path :test #'equal)) do
			  (let ((new-path (copy-seq path)))
			    (push neighbour new-path)
			    (push new-path paths)))))))
    (list longest longest-path)))


(time (let* ((input-fp "./input.txt")
	     longest
	     longest-path)

	(destructuring-bind (hashmap neighbourmap) (read-input input-fp)
	  (destructuring-bind (length path) (dfs hashmap neighbourmap)
	    (setf longest length)
	    (setf longest-path path)))

	;; (let ((test-array (make-array '(23 23)
	;; 				:initial-element #\.)))
	;;   (loop for (i . j) in longest-path do
	;;     (setf (aref test-array i j) #\*))
	;;   (loop for i from 0 below (array-dimension test-array 0) do
	;;     (format t "~a~%" (coerce (loop for j from 0 below (array-dimension test-array 1)
	;; 				   collect (aref test-array i j))
	;; 			     'string))))

  (format t "Longest path: ~s~%" longest)))

;; 2230 is the correct answer
;; Run time is about 6.5 seconds for part 1
