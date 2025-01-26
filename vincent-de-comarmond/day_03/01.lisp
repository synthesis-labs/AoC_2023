(defun cons-sort (a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

(defun get-neighbourhood (location-list)
  (let ((start (first location-list))
	(end (car (last location-list)))
	(neighbours '()))
    (loop for delta in '(-1 0 1) do
      (push (cons (+ delta (car start)) (1+ (cdr start))) neighbours)
      (push (cons (+ delta (car end)) (1- (cdr end))) neighbours))
    (loop for point in location-list do
      (push (cons (1- (car point)) (cdr point)) neighbours)
      (push (cons (1+ (car point)) (cdr point)) neighbours))
    neighbours))

(defun make-char-array ()
  (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))

(defun not-equal (a b)
  (not (= a b)))

(defun refine-numbers (number-map)
  (let ((current-key '())
	(current-val (make-char-array))
	(refined (make-hash-table :test 'equal))
	(sorted-keys (sort (loop for k being the hash-key of number-map collect k) 'cons-sort)))

    (loop for k in sorted-keys do
      (let ((prev (first current-key)))
	(if (and prev (or (not-equal (car k) (car prev))
			  (not-equal (cdr k) (1+ (cdr prev)))))
	    (progn
	      (setf (gethash (copy-seq current-key) refined) (parse-integer (copy-seq current-val)))
	      (setf current-key '())
	      (setf current-val  (make-char-array))))
	(push k current-key)
	(vector-push-extend (gethash k number-map) current-val)))
    (if (> (length current-key) 0)
	(setf (gethash (copy-seq current-key) refined) (parse-integer (copy-seq current-val))))
    refined))


(let ((full-number-map (make-hash-table :test 'equal))
      (symbols  '())
      (total 0)
      (number-map))

  (with-open-file (stream "./input.txt")
    (loop for row-idx from 0 for line = (read-line stream 'nil) while line do
      (loop for col-idx from 0 for char across line do
	(let ((point (cons row-idx col-idx)))

	  (if (digit-char-p char)
	      (setf (gethash point full-number-map) char)
	      (if (not (eql #\. char))
		  (push point symbols)))))))
  
  (setf number-map (refine-numbers full-number-map))
  (maphash (lambda (k v)
	     (if (intersection (get-neighbourhood k) symbols :test 'equal)
		 (setf total (+ total v))))
	   number-map)
  (print total))

