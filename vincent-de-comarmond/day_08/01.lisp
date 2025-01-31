(setf *print-circle* t)

(defun read-map (input-stream &optional map)
  (setf map (if (null map)
		(make-hash-table :test 'equal)
		map))
  (let ((line (read-line input-stream 'nil)))
    (if (null line)
	map
	(let ((key (subseq line 0 3))
	      (left (subseq line 7 10))
	      (right (subseq line 12 15)))
	  (setf (gethash key map) (list left right))
	  (read-map input-stream map)))))

(defun walk-through-desert (instructions map &optional (location "AAA") (step 0))
  (if (equal location "ZZZ")
      step
      (walk-through-desert instructions map
			   (nth (nth step instructions) (gethash location map))
			   (1+ step))))

(let ((input-fp "./input.txt")
      (instructions)
      (map))
  
  (with-open-file (stream input-fp)
    (setf instructions (loop for c across (read-line stream)
			     collect (if (eql c #\R) 1 0)))
    (setf (cdr (last instructions)) instructions)
    (read-line stream 'nil)
    (setf map (read-map stream)))

  (format t "Steps to salvation: ~s~%" (walk-through-desert instructions map)))

;; 19783 is the right answer for part 1
