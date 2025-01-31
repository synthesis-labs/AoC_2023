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


(defun make-paths (instructions map loc step accum)
  (let* ((instruct-no (mod step (length instructions)))
	 (key (cons loc instruct-no)))
    (if (null (gethash key accum))
	(progn (setf (gethash key accum) step)
	       (make-paths instructions
			   map
			   (nth (nth instruct-no instructions) (gethash loc map))
			   (1+ step)
			   accum))
	accum)))


(defun filter-path-table (input-h-table)
  (let ((output-h-table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
	       (if (equal (char (car k) 2) #\Z)
		   (setf (gethash k output-h-table) v)))
	     input-h-table)
    output-h-table))


(let ((input-fp "./input.txt")
      (instructions)
      (map)
      (start-locations '())
      (sub-loops)
      (lcm-candidates '()))

  (with-open-file (stream input-fp)
    (setf instructions (loop for c across (read-line stream)
			     collect (if (eql c #\R) 1 0)))
    (read-line stream 'nil)
    (setf map (read-map stream)))

  (maphash (lambda (k v)
	     (declare (ignore v))
	     (if (eql (char k 2) #\A) (push k start-locations)))
	   map)

  (setf sub-loops (mapcar (lambda (x) (filter-path-table
				       (make-paths instructions
						   map
						   x
						   0
						   (make-hash-table :test 'equal))))
			  start-locations))

  (loop for journey in sub-loops do
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (setf lcm-candidates (adjoin v lcm-candidates)))
	     journey))

  ;;; Mkay ... common lisp just gives you the lcm function for free ... ? surprised I am
  (format t "Ghost survival steps: ~s~%" (apply 'lcm lcm-candidates)))

;; 9177460370549 is the right answer for part 2
