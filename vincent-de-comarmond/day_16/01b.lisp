(defun read-grid (input-fp)
  (let ((rows 0)
	(cols 0)
	(grid (make-hash-table :test 'equal :size 150000)))

    (with-open-file (stream input-fp)
      (loop for row from 0 for line = (read-line stream 'nil)
	    while line do
	      (setf cols (length line))
	      (loop for col from 0 for char_ across line do
		(if (not (eql char_ #\.))
		    (setf (gethash (cons row col) grid) char_)))
	      (setf rows row)))
    (list grid (1+ rows) cols)))


(defun cons+ (cons1 cons2)
  (cons (+ (car cons1) (car cons2)) (+ (cdr cons1) (cdr cons2))))


(defun trace-ray (grid rows cols start dir)
  (let ((ray (list (list start (gethash start grid))))
	(stop (cond ((equal dir '(0 . 1))  (cons (car start) (1- cols)))
		    ((equal dir '(0 . -1)) (cons (car start) 0))
		    ((equal dir '(1 . 0))  (cons (1- rows) (cdr start)))
		    ((equal dir '(-1 . 0)) (cons 0 (cdr start))))))

    (do ((pos start)) ('nil)
      (let ((found (gethash pos grid))
	    (deflects 'nil))
	(case found
	  (#\\ (setf deflects 'T))
	  (#\/ (setf deflects 'T))
	  (#\| (if (= 1 (abs (cdr dir))) (setf deflects 'T)))
	  (#\- (if (= 1 (abs (car dir))) (setf deflects 'T))))

	(when (and deflects (not (equal pos start)))
	  (push (list pos found) ray)
	  (return)))

      (when (equal pos stop)
	(push (list pos 'nil) ray)
	(return))
      (setf pos (cons+ pos dir)))
    (nreverse ray)))


(defun redirect (direction reflector)
  (case reflector
    (#\\ (list (cons (cdr direction) (car direction))))
    (#\/ (list (cons (- (cdr direction)) (- (car direction)))))
    (#\- (if (not (= 0 (car direction)))
	     '((0 . -1) (0 . 1))))
    (#\| (if (not (= 0 (cdr direction)))
	     '((-1 . 0) (1 . 0))))))


(defun solve-line-segments (grid rows cols start-pos start-dir)
  (let ((active (list (list start-pos start-dir)))
	(ray '())
	(line-segments '()))

    (loop while (> (length active) 0) do
      (destructuring-bind (start dir) (pop active)
	(setf ray (trace-ray grid rows cols start dir))
	(when (not (member ray line-segments :test 'equal))
	  (push ray line-segments)
	  (destructuring-bind (end reflector) (cadr ray)
	    (loop for reflection in (redirect dir reflector) do
	      (push (list end reflection) active))))))
    line-segments))


(defun cons-sort (a b)
  (cond ((< (car a) (car b)) 'T)
	((= (car a) (car b))
	 (if (< (cdr a) (cdr b)) 'T))))

(defun determine-energized (line-segments rows cols)
  (let* ((segments (mapcar (lambda (details) (mapcar 'car details)) line-segments))
	 (grid-ids '()))

    (loop for segment in segments do
      (destructuring-bind (start end) (sort segment 'cons-sort)
	(loop for i from (car start) to (car end) do
	  (loop for j from (cdr start) to (cdr end) do
	    (push (cons i j) grid-ids)))))
    (setf grid-ids (remove-duplicates grid-ids :test #'equal))

    (flet ((out-of-bounds (grid-id)
	     (destructuring-bind (i . j) grid-id
	       (or (< i 0)
		   (>= i rows)
		   (< j 0)
		   (>= j cols)))))
      (remove-if (lambda (grid-id) (out-of-bounds grid-id)) grid-ids))))


(destructuring-bind (grid rows cols) (read-grid "./input.txt")
  (let* ((soln (solve-line-segments grid rows cols '(0 . -1) '(0 . 1)))
	 (energized (determine-energized soln rows cols)))
    (format t "Number energized: ~s~%" (length energized))))

;; 7185 is the right answer for part 1
