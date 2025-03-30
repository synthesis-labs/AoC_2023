(defun split (input-string input-char &optional (accum '()))
  (let ((pos (position input-char input-string)))
    (cond ((= 0 (length input-string)) (nreverse
					(push "" accum)))
	  ((null pos) (nreverse
		       (push input-string accum)))
	  ('t (split (subseq input-string (1+ pos))
		     input-char
		     (push (subseq input-string 0 pos) accum))))))


(defun read-input (input-fp)
  (let ((instructions '()))
    (with-open-file (stream input-fp)
      (loop for line = (read-line stream 'nil) while line do
	(destructuring-bind (dir num colour) (split line #\space)
	  (declare (ignore colour))
	  (push (list (char dir 0) (parse-integer num)) instructions))))
    (nreverse instructions)))


(defun instructions-2-segments (instructions)
  (let ((i 0)
	(j 0)
	(complete '()))

    (loop for instruction in instructions do
      (destructuring-bind (dir num) instruction
	(let ((ii (case dir
		    (#\U (- i num))
		    (#\D (+ i num))
		    (otherwise i)))
	      (jj (case dir
		    (#\L (- j num))
		    (#\R (+ j num))
		    (otherwise j))))
	  (push (list :dir dir
		      :i0 (min i ii) :i1 (max i ii)
		      :j0 (min j jj) :j1 (max j jj))
		complete)
	  (setf i ii)
	  (setf j jj))))
    (nreverse complete)))


(defun get-directed-segments (segments &rest directions)
  (let ((directed-segments ()))
    (loop for dir in directions do
      (loop for seg in segments do
	(if (eql (getf seg :dir) dir)
	    (push seg directed-segments))))
    directed-segments))


(defun group-by-key (segments key-symbol)
  (let ((segments-table (make-hash-table :size (ceiling (* 1.3 (length segments))))))
    (loop for segment in segments do
      (let ((key (getf segment key-symbol)))
	(if (gethash key segments-table)
	    (push segment (gethash key segments-table))
	    (setf (gethash key segments-table) (list segment)))))
    segments-table))

  
(defun merge-vert-line-segs (vert-line-segs &optional (sorted 'nil) (accum '()))
  (cond ((null sorted)
	 (merge-vert-line-segs (sort vert-line-segs (lambda (a b) (< (car a) (car b))))
			       'T
			       accum))
	((null accum)
	 (merge-vert-line-segs (rest vert-line-segs)
			       sorted
			       (push (car vert-line-segs) accum)))
	((null vert-line-segs)
	 (reverse accum))

	('T (let* ((current (pop vert-line-segs))
		   (head (car current))
		   (tail (cadr current))
		   (prev (cadr (car accum))))

	      (if (<= head prev)
		  (if (< prev tail)
		      (setf (cadr (car accum)) tail))
		  (push current accum))

	      (merge-vert-line-segs vert-line-segs sorted accum)))))

  
(defun yolo (segments)
  (let* ((total 0)
	 (horiz-segs (sort (get-directed-segments segments #\R #\L)
			   (lambda (a b) (< (getf a :i0) (getf b :i0)))))
	 (min-j (apply 'min (mapcar (lambda (x) (getf x :j0)) horiz-segs)))
	 (max-j (apply 'max (mapcar (lambda (x) (getf x :j1)) horiz-segs)))
	 (vert-segs (get-directed-segments segments #\U #\D))
	 (vert-table (group-by-key vert-segs ':j0)))

    (loop for j from min-j to max-j do
      (let* ((vert-lines '())
	     (horiz-subset (remove-if (lambda (x)
					(or (< j (getf x :j0)) (< (getf x :j1) j)))
				      horiz-segs))
	     (right-subset (remove-if-not (lambda (x)
					    (eql #\R (getf x :dir)))
					  horiz-subset)))

	(loop for top-seg in right-subset do
	  (let ((lower-subset (remove-if (lambda (x)
					   (<= (getf x :i0) (getf top-seg :i0)))
					 horiz-subset)))
	    (loop for bot-seg in lower-subset do
	      (push (list (getf top-seg :i0) (getf bot-seg :i0)) vert-lines)
	      (return))))

	(loop for seg in (gethash j vert-table) do
	  (push (list (getf seg :i0) (getf seg :i1)) vert-lines))

	(loop for segment in (merge-vert-line-segs vert-lines) do
	  (setf total (+ total (- (cadr segment) (car segment)) 1)))))

    total))


(time (let* ((input-fp "./input.txt")
	     (instructions (read-input input-fp))
	     (segments (instructions-2-segments instructions))
	     (total (yolo segments)))
	(format t "Total lake volume: ~s~%" total)))


;; 40664 is too low for part 1
;; 47417 is still too low for part 1
 
;; 52035 is the correct answer for part 1

;; 51985 is the v0 answer

;; 52035 is the correct answer for part 1
;; Now super quick ... ~ 0.02 s
