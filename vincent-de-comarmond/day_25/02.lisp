(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))


(defun parse-line (line)
  (destructuring-bind (key values) (split-str line #\:)
    (list key (split-str values #\ ))))


(defun read-input (input-fp)
  (let (relationships)
    (with-open-file (stream input-fp)
      (loop for idx from 0
	    for line = (read-line stream 'nil) while line do
	      (destructuring-bind (key values) (parse-line line)
		(loop for v in values if (not (equal v "")) do
		  (setf relationships (adjoin (sort (list key v) #'string<)
					      relationships
					      :test #'equal))))))
    relationships))


(defun random-choice (input-list) 
  (nth (random (length input-list)) input-list))


(defun print-map (input-map)
  (maphash (lambda (k v) (format t "~a: ~a~%" k v)) input-map))


;;;; This is the slow part
(defun organic-split (vertices edges min-split-size)
  (let* ((seed (random-choice vertices))
	 (split (list seed))
	 (counts (make-hash-table :test #'equal)))

    (loop named outer while (< (length split) min-split-size) do
      (loop for vertex in (set-difference vertices split :test #'equal) do
	(loop for (a b) in edges if (or (and (equal a vertex)
					     (member b split :test #'equal))
					(and (equal b vertex)
					     (member a split :test #'equal)))
	      do (if (equal a vertex)
		     (incf (gethash a counts 0))
		     (incf (gethash b counts 0)))))
      (let* ((most (apply 'max (loop for v being the hash-values of counts collect v)))
	     (best (loop for k being the hash-keys of counts
			 if (equal most (gethash k counts)) do
			 (return k))))
	(setf split (adjoin best split :test #'equal)))
      (clrhash counts))
    split))


(defun partition (vertices edges)
  (let* ((subset-a (organic-split vertices edges (floor (length vertices) 4)))
	 (subset-b (set-difference vertices subset-a :test #'equal))
	 (most-connected (make-hash-table :test #'equal))
	 (least-connected (make-hash-table :test #'equal)))

    (loop for idx from 1 to 10 do
      (format t "Generation: ~s~%" idx)
      (clrhash most-connected)
      (clrhash least-connected)

      (loop for vertex in subset-b do
	(loop for edge in edges if (and (member vertex edge :test #'equal)
					(intersection edge subset-a :test #'equal))
	      do (incf (gethash vertex most-connected 0))))

      (loop for vertex in subset-a do
	(loop for edge in edges if (and (member vertex edge :test #'equal)
					(= 2 (length (intersection edge subset-a :test #'equal))))
	      do (incf (gethash vertex least-connected 0))))

      ;; Add any connections between a and b that are more than 1 connected
      (maphash
       (lambda (k v)
	 (when (< 1 v)
	   (setf subset-a (adjoin k subset-a :test #'equal))
	   (setf subset-b (remove k subset-b :test #'equal))))
       most-connected)

      ;; Remove any connections between a and a that are only 1 connected
      (maphash
       (lambda (k v)
	 (when (< v 2)
	   (setf subset-a (remove k subset-a :test #'equal))
	   (setf subset-b (adjoin k subset-b :test #'equal))))
       least-connected)

      (if (and (= 3 (hash-table-count most-connected))
	       (= 3 (loop for v being the hash-values of most-connected sum v)))
	  (return)))

    (list subset-a subset-b)))


(let* ((input-fp "./input.txt")
       (relations (read-input input-fp))
       (vertices (remove-duplicates (loop for el in (copy-tree relations) nconcing el)
				    :test #'equal))
       (partitions (partition vertices relations))
       (partition-power (apply '* (mapcar 'length partitions))))

  (format t "Partition power is: ~s~%" partition-power))

;; Solution is easily 614655
;; Far more quickly than expected
