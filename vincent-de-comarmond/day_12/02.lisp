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


(defun new-fmt-possibilities (new-fmt num-springs &optional (collected '()) (idx 0))
  (let ((new-list '())
	(total (- num-springs (apply '+ new-fmt))))

    (cond ((= idx (length new-fmt))
	   (loop for baby-list in collected do
	     (push (nreverse (cons
			      (- total (apply '+ baby-list))
			      (copy-seq baby-list)))
		   new-list))
	   ;; return value
	   new-list)

	  ((= idx 0)
	   (new-fmt-possibilities new-fmt
			      num-springs
			      (loop for i from 0 to total collect (list i))
			      1))

	  (t (loop for baby-list in collected
		   do (loop for i from 1 to (- total (apply '+ baby-list))
			    do (push (cons i (copy-seq baby-list)) new-list)))
	     (new-fmt-possibilities new-fmt num-springs new-list (1+ idx))))))


(defun interpolate (new-fmt new-possibilities)
  (let ((interpolated '()))
    (loop for new in new-possibilities do
      (push (append (mapcan (lambda (a b) (list a b)) new new-fmt)
		    (last new))
	    interpolated))
    (mapcar (lambda (sublist)
	      (loop for x in sublist sum x into y collect y))
	    interpolated)))


(defun make-checkers (interpolation)
  (let ((curr 0)
	(prev 0)
	(defunct '())
	(funct '()))

    (loop for idx from 0 to (1- (length interpolation)) do
      (setf curr (nth idx interpolation))

      (if (> idx 0)
	  (setf prev (nth (1- idx) interpolation))
	  (setf prev 0))

      (if (= 0 (mod idx 2))
	  (push (list prev curr) defunct)
	  (push (list prev curr) funct)))

    (list (nreverse defunct) (nreverse funct))))


(defun check (idx checker)
  (loop for subsection in checker do
    (destructuring-bind (start end) subsection
      (if (and (>= idx start) (< idx end))
	  (return-from check 'T)))))


(defun determine-num-possibilities (old-new-format)
  (let* ((old-fmt (car old-new-format))
	 (new-fmt (cdr old-new-format))
	 (total-springs (length old-fmt))
	 (new-possibilities (new-fmt-possibilities new-fmt total-springs))
	 (interpolations (interpolate new-fmt new-possibilities))
	 (passed 'nil)
	 (possibilities 0))

    (loop for interpolated in interpolations do
      (setf passed 'T)
      (destructuring-bind (broken-check working-check) (make-checkers interpolated)
	(loop for idx from 0 for ch across old-fmt do
	  (if passed
	      (case ch
		(#\?) ;; do nothing
		(#\. (setf passed (check idx broken-check)))
		(#\# (setf passed (check idx working-check))))))
	(if passed (incf possibilities))))
    possibilities))


(let ((total 0)
      (parsed-line))
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream 'nil)
	  while line do
	    (setf parsed-line (read-process-line line))
	    (setf total (+ total (determine-num-possibilities parsed-line)))))
  (format t "Total possibilities: ~s~%" total))

;; 8193 is the right answer for part 1
