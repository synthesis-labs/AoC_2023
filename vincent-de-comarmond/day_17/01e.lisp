(defun read-map (input-fp)
  (let ((cost-grid (make-hash-table :size (* 120 120)))
	(cols 'nil)
	(idx 0)
	(rows 0))
    (with-open-file (stream input-fp)
      (loop for char_ = (read-char stream 'nil)
	    while char_ do
	      (if (eql char_ #\newline)
		  (progn (incf rows)
			 (if (null cols) (setf cols idx)))
		  (progn (setf (gethash idx cost-grid) (digit-char-p char_))
			 (incf idx)))))
    (list cost-grid rows cols)))


(defun print-grid (grid rows cols)
  (loop for r from 0 below rows do
    (format t "~a~%" (coerce
		      (loop for c from 0 below cols
			    collect (digit-char (gethash (+ (* r cols) c) grid)))
		      'string))))


(defun make-neighbour-grid (grid rows cols)
  (let ((neighbours (make-hash-table :size (hash-table-count grid))))
    (loop for r from 0 below rows do
      (loop for c from 0 below cols do
	(let ((pos (+ (* r cols) c)) (locale '()))
	  (if (> r 0) (push (- pos cols) locale))
	  (if (< r (1- rows)) (push (+ pos cols) locale))
	  (if (> (mod c cols) 0) (push (1- pos) locale))
	  (if (< (mod c cols) (1- cols)) (push (1+ pos) locale))

	  (setf (gethash pos neighbours) locale))))
    neighbours))


(defun solve (grid rows cols)
  (let ((neighbour-grid (make-neighbour-grid grid rows cols))
	(target (1- (* rows cols)))
	(optimal (make-hash-table :size (* 9 4 (hash-table-count grid)) :test #'equal))
	(active '((:pos 0 :dir 0 :num 0 :loss 0))))

    (loop while (> (length active) 0) do
      (let* ((losses (mapcar (lambda (x) (getf x :loss)) active))
	     (best-pos (position (apply 'min losses) losses))
	     (best (nth best-pos active)))

	(setf active (loop for idx from 0 for head in active
			   if (not (= idx best-pos)) collect head))

	(destructuring-bind (&key pos dir num loss) best

	  (when (null (gethash (list pos dir num) optimal))
	    (setf (gethash (list pos dir num) optimal) loss)

	    (loop for neighbour in (gethash pos neighbour-grid) do
	      (let ((direction (- neighbour pos))
		    (next-gen 'nil)
		    (new-loss (+ loss (gethash neighbour grid))))

		(cond ((= (- dir) direction)) ;; do nothing - can't go backwards
		      ((and (= num 3)
			    (= dir direction))) ;; do nothing - 3 in a row rule
		      ((not (= dir direction))
		       (setf next-gen (list :pos neighbour
					    :dir direction
					    :num 1
					    :loss new-loss)))
		      ('T
		       (setf next-gen (list :pos neighbour
					    :dir dir
					    :num (+ num 1)
					    :loss new-loss))))

		(when (not (null next-gen))
		  (push next-gen active)
		  (if (= (getf next-gen :pos) target)
		      (return-from solve next-gen)))))))))))


(destructuring-bind (grid rows cols) (read-map "./input.txt")
  (format t "~s~%" (solve grid rows cols)))

;; 907 is the right answer for part 1
;; takes about 45 seconds ... so hugely slow
