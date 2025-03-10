(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))


(defun read-process-line (input-line &optional (multiplier 1))
  (let* ((middle (position #\Space input-line))
	 (old-format (subseq input-line 0 middle))
	 (old-format (if (> multiplier 1)
			 (format nil "~v@{~A?~:*~}" multiplier old-format)
			 old-format))
	 (old-format (if (> multiplier 1)
			 (subseq old-format 0 (1- (length old-format)))
			 old-format))
	 (new-format (subseq input-line (1+ middle)))
	 (new-format (string-right-trim "," (format nil "~v@{~A,~:*~}" multiplier new-format)))
	 (parsed-new (mapcar 'parse-integer (split-str new-format #\,))))
    (cons old-format parsed-new)))


(defparameter *generation-cache* (make-hash-table :test 'equal :size 50000))
(defun make-next-generation (spring-group old-fmt)
  ;; (format t "~cIndex ~s of ~s~%" #\tab index (length old-fmt))
  (let* ((key (cons spring-group old-fmt))
	 (next-generation '())
	 (len (length old-fmt))
	 (size spring-group)
	 (unfinished 'T))

    ;; too simple to hash
    (if (< len size)
	(return-from make-next-generation 'nil))

    (multiple-value-bind (hash-val key-present) (gethash key *generation-cache*)
      (when key-present
	(return-from make-next-generation hash-val)))

    (loop for start-pos from 0 to (- len size)
	  for ch across old-fmt do
	    (let* ((end-pos (+ start-pos size))
		   (target-substr (subseq old-fmt start-pos end-pos))
		   (next-char (if (< end-pos  len) (char old-fmt  end-pos)))
		   (is-valid (and (null (position #\. target-substr))
				  (or (null next-char)
				      (not (eql next-char #\#))))))

	      (when (or (eql ch #\?) (eql ch #\#))
		(when (and is-valid unfinished)
		  (if (< (1+ end-pos) len)
		      (push (subseq old-fmt (1+ end-pos)) next-generation)
		      (push '() next-generation)))

		(if (eql ch #\#) (setf unfinished 'nil)))))

    (setf (gethash key *generation-cache*) next-generation)
    next-generation))


(defparameter *single-soln-cache* (make-hash-table :test 'equal :size 50000))
(defun solve-single (old-fmt-new-fmt &optional (accum 0))
  (let* ((key old-fmt-new-fmt)
	 (old-fmt (car old-fmt-new-fmt))
	 (new-fmt (cdr old-fmt-new-fmt))
	 (cached-result (gethash key *single-soln-cache*))
	 (result 'nil)
	 (next-generation 'nil))

    (if cached-result
	(return-from solve-single (+ accum cached-result)))

    (setf next-generation (make-next-generation (car new-fmt) old-fmt))
    (if (null (rest new-fmt))
	(progn (setf result (length (remove-if
				     (lambda (soln) (find #\# soln))
				     next-generation)))
	       (setf result (+ result accum)))
	(progn (setf next-generation (remove 'nil next-generation))
	       (setf next-generation
		     (mapcar (lambda (x) (cons x (rest new-fmt))) next-generation))
	       (setf result (mapcar 'solve-single next-generation))
	       (setf result (+ accum (apply '+ result)))))
    (setf (gethash key *single-soln-cache*) result)
    result))


(let ((total 0)
      (processed-line 'nil)
      (subtotal 0))

  (with-open-file (stream "./input.txt")
    (loop for line-num from 1
	  for line = (read-line stream 'nil)
	  while line do
	    ;; (format t "Processing line ~s of 1000~%" line-num)
	    (setf processed-line (read-process-line line 5))
	    (setf subtotal (solve-single processed-line))
	    (setf total (+ total subtotal)))
    (format t "Total possibilities: ~s~%" total)))

;; 8193 is the right answer for part 1
;; 45322533163795 is the right answer for part 2
