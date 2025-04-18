(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))

(defun parse-line (idx line)
  (destructuring-bind (start end) (split-str line #\~)
    (destructuring-bind ((x0 y0 z0) (x1 y1 z1))
	(mapcar (lambda (x) (mapcar 'parse-integer (split-str x #\,))) (list start end))
      (list :id idx :x0 x0 :y0 y0 :z0 z0 :x1 x1 :y1 y1 :z1 z1))))

(defun read-input (input-fp)
  (with-open-file (stream input-fp)
    (loop for idx from 0
	  for line = (read-line stream 'nil)  while line
	  collect (parse-line idx line))))

(defun z0-sort (a b) (< (getf a :z0) (getf b :z0)))

(defun fall (input-blocks)
  (let ((heights (make-hash-table :size (* 2 (length input-blocks)) :test #'equal))
	(fallen (make-hash-table :size (* 2 (length input-blocks))))
	(active (sort (copy-seq input-blocks) 'z0-sort)))

    (loop while (> (length active) 0) do
      (destructuring-bind (&key id x0 y0 z0 x1 y1 z1) (pop active)
	(let ((max-height 0))
	  (loop for x from x0 to x1 do
	    (loop for y from y0 to y1 do
	      (setf max-height (max max-height (gethash (list x y) heights 0)))))
	
	  (loop for x from x0 to x1 do
	    (loop for y from y0 to y1 do
	      (setf (gethash (list x y) heights) (+ max-height (- z1 z0) 1))))

	  (setf (gethash id fallen)
		(list :x0 x0 :y0 y0 :z0 (+ max-height 1)
		      :x1 x1 :y1 y1 :z1 (+ max-height (- z1 z0) 1)))))

      (setf active (sort (copy-seq active) 'z0-sort)))
    fallen))


(defun determine-supporters (fallen-blocks)
  (let ((supporters (make-hash-table)))

    (maphash (lambda (id properties)
	       (destructuring-bind (&key x0 y0 z0 x1 y1 z1) properties
		 (declare (ignore z1))
		 (maphash
		  (lambda (id_ p)
		    (if (and (/= id id_)
			     (= z0 (1+ (getf p :z1)))
			     (<= x0 (getf p :x1))
			     (<= (getf p :x0) x1)
			     (<= y0 (getf p :y1))
			     (<= (getf p :y0) y1))
			(push id_ (gethash id supporters '()))))
		  fallen-blocks)))
	     fallen-blocks)
    supporters))

(defun count-removable (fallen-blocks supporters)
  (let* ((single-supports (loop for v being the hash-values of supporters
				if (= 1 (length v)) collect (car v)))
	(single-supports (remove-duplicates single-supports))
	(total 0))

    (loop for k being the hash-keys of fallen-blocks do
      (if (not (member k single-supports))
	  (incf total)))
    total))



(time (let* ((input (read-input "./input.txt"))
	     (fallen (fall input))
	     (supporters (determine-supporters fallen)))
	;; (format t "fallen: ~s~%" fallen)
	;; (maphash (lambda (k v) (format t "~s: ~s~%" k v)) fallen)
	;; (format t "Supporters: ~s~%" supporters)
	;; (maphash (lambda (k v) (format t "~s: ~s~%" k v)) supporters)
	(format t "Number removable: ~s~%" (count-removable fallen supporters))))

;; 488 is the right answer for part 1
;; run-time is like 0.05 s
