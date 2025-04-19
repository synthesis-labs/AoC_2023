(defun read-input (input-fp)
  (let ((map_ (make-hash-table :size (* 150 150) :test #'equal))
	(neighbours (make-hash-table :size (* 150 150) :test #'equal))
	points
	rows
	cols
	start
	end)

    (with-open-file (stream input-fp)
      (loop for i from 0
	    for line = (read-line stream 'nil)
	    while line do
	      (loop for j from 0
		    for char_ across line do
		      (if (not (eql char_ #\#))
			  (setf (gethash (cons i j) map_) char_)))))

    (setf points (loop for k being the hash-keys of map_ collect k))
    (setf rows (mapcar 'car points))
    (setf cols (mapcar 'cdr points))
    (setf start (cons (apply 'min rows) (apply 'min cols)))
    (setf end (cons (apply 'max rows) (apply 'max cols)))

    (loop for k being the hash-keys of map_ do
      (destructuring-bind (i . j) k
	(setf (gethash k neighbours) '())
	(loop for neighbour in (list (cons (1- i) j)
				     (cons i (1+ j))
				     (cons i (1- j))
				     (cons (1+ i) j))
	      if (gethash neighbour map_) do
		(push neighbour (gethash k neighbours)))))
    (list :grid map_ :neighbour-map neighbours :start start :end end)))


(defun determine-decision-points (neighbour-map)
  ;; Every decision path leads either to the start, or to the end, or to another decision path
  (let (decision-points)
    (maphash (lambda (k v)
	       (if (> (length v) 2)
		   (push k decision-points)))
	     neighbour-map)
    decision-points))


(defun find-paths (neighbour-map decision-points start end)
  (let* ((all-crit-pts (append decision-points (list start end)))
	 (all-crit-pts (remove-duplicates all-crit-pts :test #'equal))
	 (results (make-hash-table :size (* (length all-crit-pts) (length all-crit-pts))
				   :test #'equalp)))

    (loop for crit-pt in all-crit-pts do
      (let ((paths (list (list crit-pt)))
	    (finished '()))

	(loop while (< 0 (length paths)) do
	  (let* ((path (pop paths))
		 (head (car path))
		 (neighbours (gethash head neighbour-map)))

	    (loop for neighbour in neighbours
		  if (not (member neighbour path :test #'equal)) do
		    (let ((new-path (copy-seq path)))
		      (push neighbour new-path)
		      (if (member neighbour all-crit-pts :test #'equal)
			  (push new-path finished)
			  (push new-path paths))))))

	(setf (gethash crit-pt results) (make-hash-table
					 :test #'equal :size (length all-crit-pts)))

	(loop for complete-path in finished do
	  (setf (gethash (car complete-path)
			 (gethash crit-pt results))
		(1- (length complete-path))))))
    results))

(defun calc-crit-dist (crit-path-distances path)
  (let ((cum-dist 0))
    (loop for (b a) on path if a do
      (setf cum-dist (+ cum-dist (gethash b (gethash a crit-path-distances)))))
    cum-dist))


(defun find-longest-hike (crit-paths start end)
  (let ((longest 0)
	(paths (list (list start))))

    (loop while (< 0 (length paths)) do
      (let* ((path (pop paths))
	     (head (car path)))

	(loop for neighbour being the hash-keys of (gethash head crit-paths)
	      if (not (member neighbour path :test #'equal)) do
		(let ((new-path (copy-seq path)))
		  (push neighbour new-path)
		  (if (equal neighbour end)
		      (let ((dist (calc-crit-dist crit-paths new-path)))
			(setf longest (max longest dist)))
		      (push new-path paths))))))

    longest))


(time (let* ((input-fp "./input.txt")
	     critical-points
	     critical-paths
	     longest-hike)

	(destructuring-bind (&key grid neighbour-map start end) (read-input input-fp)
	  (declare (ignore grid))
	  (setf critical-points (determine-decision-points neighbour-map))
	  (setf critical-paths (find-paths neighbour-map
					   critical-points
					   start
					   end))
	  (setf longest-hike (find-longest-hike critical-paths start end)))

	(format t "Max hike distance: ~s~%" longest-hike)))

;; 6542 is the right answer for part 2
;; run-time is about 10s
