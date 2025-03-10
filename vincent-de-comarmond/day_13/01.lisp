(defun get-rows-cols (hashmap)
  (let* ((keys (loop for k being the hash-key of hashmap collect k))
	 (rows (sort (remove-duplicates (mapcar 'car keys)) '<))
	 (cols (sort (remove-duplicates (mapcar 'cdr keys)) '<)))
    (list rows cols)))


(defun print-map (valley)
  (destructuring-bind (rows cols) (get-rows-cols valley)
    (loop for row in rows do
      (format t "~s~%" (coerce (loop for col in cols
				     collect (gethash (cons row col) valley))
			       'string)))))


(defun solve-horiz (valley)
  (destructuring-bind (rows cols) (get-rows-cols valley)
    (let* ((symmetric 'nil)
	   (symmetries '())
	   (max-row (apply 'max rows)))

      (loop for row from 0 to (1- max-row) do
	(setf symmetric 'T)
	(loop for delta from 0 to row do
	  (let ((up (- row delta))
		(down (+ row delta 1))
		(above 'nil)
		(below 'nil))
	    (when (and (<= down max-row) (<= 0 up) symmetric)
	      (setf above (coerce (loop for col in cols collect (gethash (cons up col) valley))
				  'string))
	      (setf below (coerce (loop for col in cols collect (gethash (cons down col) valley))
				  'string))
	      (if (not (equal above below))
		  (setf symmetric 'nil)))))
	(if symmetric (push (1+ row) symmetries)))
      symmetries)))


(defun solve-vert (valley)
  (destructuring-bind (rows cols) (get-rows-cols valley)
    (let* ((symmetric 'nil)
	   (symmetries '())
	   (max-col (apply 'max cols)))

      (loop for col from 0 to (1- max-col) do
	(setf symmetric 'T)
	(loop for delta from 0 to col do
	  (let ((left (- col delta))
		(right (+ col delta 1))
		(lh 'nil)
		(rh 'nil))
	    (when (and (<= right max-col) (<= 0 left) symmetric)
	    (setf lh (loop for row in rows collect (gethash (cons row left) valley)))
	    (setf rh (loop for row in rows collect (gethash (cons row right) valley)))
	    (if (not (equal lh rh))
		(setf symmetric 'nil)))))

	(if symmetric (push (1+ col) symmetries)))
      symmetries)))


(defun solve-valley (valley)
  (let ((subtotal-horiz (apply '+ (solve-horiz valley)))
	(subtotal-vert (apply '+ (solve-vert valley))))
    (+ (* 100 subtotal-horiz) subtotal-vert)))


(let ((valley (make-hash-table :test 'equal :size 1000))
      (total 0))
  (with-open-file (stream "./input.txt")
    (loop for row from 0
	  for line = (read-line stream 'nil) while line
	  do (if (equal line "")
		 (progn (setf total (+ total (solve-valley valley)))
			(setf row -1)
			(clrhash valley))
		 (loop for col from 0 for ch across line
		       do (setf (gethash (cons row col) valley) ch)))))
  (setf total (+ total (solve-valley valley)))
  (format t "Total: ~s~%" total))

;; 40006 is the correct answer for part 1
