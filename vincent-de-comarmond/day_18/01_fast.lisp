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
	(push (list :dir dir
		    :i0 i
		    :i1 (case dir
			  (#\U (- i num))
			  (#\D (+ i num))
			  (otherwise i))
		    :j0 j
		    :j1 (case dir
			  (#\L (- j num))
			  (#\R (+ j num))
			  (otherwise j)))
	      complete)
	(setf i (getf (car complete) :i1))
	(setf j (getf (car complete) :j1))))
    (nreverse complete)))


(defun get-all-vertical-line-segments (segments)
  (let* ((lines '())
	 (right-segments (remove-if-not (lambda (x) (eql #\R (getf x :dir)))
					segments))

	 (vertical-segs (remove-if (lambda (x) (or (eql #\R (getf x :dir))
						   (eql #\L (getf x :dir))))
				   segments))

	 (vertical-boundaries (mapcar (lambda (x) (list :i0 (min (getf x :i0) (getf x :i1))
							:i1 (max (getf x :i0) (getf x :i1))
							:j0 (min (getf x :j0) (getf x :j1))
							:j1 (max (getf x :j0) (getf x :j1))))
				      vertical-segs)))

    (loop for segment in right-segments do
      (destructuring-bind (&key i0 j0 j1 &allow-other-keys) segment

	(let ((consider-me (remove-if-not (lambda (x) (< i0 (min (getf x :i0)
								 (getf x :i1))))
					  segments)))

	  (setf consider-me (sort consider-me (lambda (a b)
						(< (min (getf a :i0) (getf a :i1))
						   (min (getf b :i0) (getf b :i1))))))

	  (loop for j from (min j0 j1) to (max j0 j1) do
	    (loop for seg in consider-me do

	      (let ((j0_ (getf seg :j0))
		    (j1_ (getf seg :j1))
		    (i0_ (min (getf seg :i0) (getf seg :i1))))

		(when (and (<= (min j0_ j1_) j) (<= j (max j0_ j1_)))
		  (push (list :i0 i0 :i1 i0_ :j0 j :j1 j) lines)
		  (return))))))))

    (setf lines (append lines vertical-boundaries))
    lines))


(defun merge-vertical-line-segments (vert-line-segs &optional (sorted 'nil) (accum '()))
  (cond ((null sorted)
	 (merge-vertical-line-segments (sort vert-line-segs
					     (lambda (a b) (< (getf a :i0) (getf b :i0))))
				       'T
				       accum))
	((null accum) (merge-vertical-line-segments (rest vert-line-segs)
						    sorted
						    (push (car vert-line-segs) accum)))
	((null vert-line-segs) (reverse accum))

	('T (let ((item (car vert-line-segs))
		  (tail-merged (getf (car accum) :i1)))
	      (if (<= (getf item :i0) tail-merged)
		  (progn (if (< tail-merged (getf item :i1))
			     (setf (getf (car accum) :i1) (getf item :i1)))
			 (merge-vertical-line-segments (rest vert-line-segs)
						       sorted
						       accum))

		  (merge-vertical-line-segments (rest vert-line-segs)
						sorted
						(push item accum)))))))


(defun merge-all-vertical-lines (all-vertical-lines min-j max-j)
  (let* ((merged-lines '()))

    (loop for j from min-j to max-j do
      (let ((subset (remove-if (lambda (x) (/= j (getf x :j0))) all-vertical-lines)))
	(loop for merged in (merge-vertical-line-segments subset) do
	  (push merged merged-lines))))

    (remove-duplicates merged-lines :test #'equalp)))


(defun count-lines (all-line-segments)
  (let ((total 0))
    (loop for line-segment in all-line-segments do
      (destructuring-bind (&key i0 i1 j0 j1) line-segment
	  (setf total (+ total (abs (- i1 i0)) (abs (- j1 j0)) 1))))
    total))


(time (let* ((input-fp "./input.txt")
	     (instructions (read-input input-fp))
	     (segments (instructions-2-segments instructions))
	     (min-j (apply 'min (mapcar (lambda (x) (min (getf x :j0) (getf x :j1))) segments)))
	     (max-j (apply 'max (mapcar (lambda (x) (max (getf x :j0) (getf x :j1))) segments)))
	     (all-line-segments (get-all-vertical-line-segments segments))
	     (merged-lines (merge-all-vertical-lines all-line-segments min-j max-j))
	     (volume (count-lines merged-lines)))

	(format t "Total lake volume: ~s~%" volume)))


;; 40664 is too low for part 1
;; 47417 is still too low for part 1
 
;; 52035 is the correct answer for part 1

;; 51985 is the v0 answer

;; 52035 is the correct answer for part 1
;; Now super quick ... ~ 0.02 s
