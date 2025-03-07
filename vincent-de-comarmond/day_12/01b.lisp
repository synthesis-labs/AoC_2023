(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))


(defun read-process-line (input-line)
  (let* ((middle (position #\Space input-line))
	 (old-format (subseq input-line 0 middle))
	 (new-format (subseq input-line (1+ middle)))
	 (parsed-new (mapcar 'parse-integer (split-str new-format #\,))))
    (cons old-format parsed-new)))


(defun make-repeated (repeat-num repeat-str)
  (format nil "~v@{~A~:*~}" repeat-num repeat-str))


(defun make-next-generation (spring-group old-fmt index)
  (let ((next-generation '())
	(len (length old-fmt))
	(size spring-group)
	(unfinished 'T))

    (if (<= len index)
	(return-from make-next-generation '()))

    (loop for start-pos from index to (- len size)
	  for ch across (subseq old-fmt index) do
	    (let* ((end-pos (+ start-pos size))
		   (target-substr (subseq old-fmt start-pos end-pos))
		   (next-char (if (< end-pos (1- len)) (char old-fmt  end-pos)))
		   (is-valid (and (null (position #\. target-substr))
				  (or (null next-char)
				      (not (eql next-char #\#))))))

	      (when (or (eql ch #\?) (eql ch #\#))
		(when (and is-valid unfinished)
		  (if (eql ch #\#) (setf unfinished 'nil))

		  (let ((og-copy (substitute #\. #\? old-fmt :end start-pos)))
		    (setf (subseq og-copy start-pos end-pos) (make-repeated size #\#))
		    (if next-char (setf (aref og-copy end-pos) #\.))
		    (push (cons og-copy (1+ end-pos)) next-generation))))))
    next-generation))


(defun filter-invalids (partial-complete-array spring-groups)
  (let* ((spring-number (length partial-complete-array))
	 (required-active (apply '+ spring-groups))
	 (known-active (count #\# partial-complete-array))
	 (known-inactive (count #\. partial-complete-array))
	 (unknowns (- spring-number known-active known-inactive)))
    (cond
      ((< (+ known-active unknowns) required-active) 'T)
      ((> known-active required-active) 'T)
      ('T 'nil))))

(defun solve (old-fmt spring-groups)
  (let ((generations (list (cons old-fmt 0)))
	(next-generations '()))

    (loop for new-fmt in spring-groups do
      (loop for current-gen-idx in generations do
	(destructuring-bind (curr-gen . start-idx) current-gen-idx
	  (loop for next-gen in (make-next-generation new-fmt curr-gen start-idx) do
	    (if next-gen
		(push next-gen next-generations)))))
      (setf generations (remove-if
			 (lambda (possibility)
			   (filter-invalids (car possibility) spring-groups))
			 next-generations))
      (setf next-generations '()))
    generations))


(let ((total 0)
      (subtotal 0))
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream 'nil)
	  while line do
	    (destructuring-bind (old-fmt . new-fmt) (read-process-line line)
	      (setf subtotal (length (solve old-fmt new-fmt)))
	      (setf total (+ total subtotal)))))
  
  (format t "Total possibilities: ~s~%" total))

;; 8193 is the right answer for part 1
