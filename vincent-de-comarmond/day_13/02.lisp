(defun get-rows-cols (hashmap)
  (let* ((keys (loop for k being the hash-key of hashmap collect k))
	 (rows (sort (remove-duplicates (mapcar 'car keys)) '<))
	 (cols (sort (remove-duplicates (mapcar 'cdr keys)) '<)))
    (list rows cols)))


(defun count-differences (char-list-1 char-list-2)
  (let ((differences 0))
    (loop for char1 in char-list-1 for char2 in char-list-2
	  do (if (not (eql char1 char2)) (incf differences)))
    differences))


(defun solve-horiz (valley &optional (target-breakages 0))
  (destructuring-bind (rows cols) (get-rows-cols valley)
    (let* ((breakages 0)
	   (max-row (apply 'max rows)))

      (loop for row from 0 to (1- max-row) do
	(setf breakages 0)
	(loop for delta from 0 to row do
	  (let ((up (- row delta))
		(down (+ row delta 1))
		(above 'nil)
		(below 'nil))
	    (when (and (<= down max-row) (<= 0 up))
	      (setf above (loop for col in cols collect (gethash (cons up col) valley)))
	      (setf below (loop for col in cols collect (gethash (cons down col) valley)))
	      (setf breakages (+ breakages (count-differences above below))))))
	(if (= breakages target-breakages)
	    (return-from solve-horiz (1+ row))))
      0)))


(defun solve-vert (valley &optional (target-breakages 0))
  (destructuring-bind (rows cols) (get-rows-cols valley)
    (let* ((breakages 0)
	   (max-col (apply 'max cols)))

      (loop for col from 0 to (1- max-col) do
	(setf breakages 0)
	(loop for delta from 0 to col do
	  (let ((left (- col delta))
		(right (+ col delta 1))
		(lh 'nil)
		(rh 'nil))
	    (when (and (<= right max-col) (<= 0 left))
	      (setf lh (loop for row in rows collect (gethash (cons row left) valley)))
	      (setf rh (loop for row in rows collect (gethash (cons row right) valley)))
	      (setf breakages (+ breakages (count-differences lh rh))))))
	(if (= breakages target-breakages)
	    (return-from solve-vert (1+ col))))
      0)))


(defun solve-valley (valley &optional (horiz-corrections 0) (vert-corrections 0))
  (let ((subtotal-horiz (solve-horiz valley horiz-corrections))
	(subtotal-vert (solve-vert valley vert-corrections)))
    (+ (* 100 subtotal-horiz) subtotal-vert)))


(let ((valley (make-hash-table :test 'equal :size 1000))
      (horiz-smudges 1)
      (vert-smudges 1)
      (total 0))
  (with-open-file (stream "./input.txt")
    (loop for row from 0
	  for line = (read-line stream 'nil) while line
	  do (if (equal line "")
		 (progn (setf total (+ total (solve-valley valley horiz-smudges vert-smudges)))
			(setf row -1)
			(clrhash valley))
		 (loop for col from 0 for ch across line
		       do (setf (gethash (cons row col) valley) ch)))))
  (setf total (+ total (solve-valley valley horiz-smudges vert-smudges)))
  (format t "Total: ~s~%" total))

;; 40006 is the correct answer for part 1
;; 28627 is the correct answer for part 2
