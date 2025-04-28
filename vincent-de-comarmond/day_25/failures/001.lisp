(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))


(defun parse-line (line)
  (destructuring-bind (key values) (split-str line #\:)
    (list key (split-str values #\ ))))


(defun read-input (input-fp)
  (let ((relations (make-hash-table :test #'equal)))
    (with-open-file (stream input-fp)
      (loop for idx from 0
	    for line = (read-line stream 'nil) while line do
	      (destructuring-bind (key values) (parse-line line)
		(loop for v in values if (not (equal v "")) do
		  (push v (gethash key relations '()))
		  (push key (gethash v relations '()))))))
    (maphash (lambda (k v)
	       (setf (gethash k relations) (remove-duplicates v :test #'equal)))
	     relations)
    relations))


(defun print-map (input-map)
  (maphash (lambda (k v) (format t "~s: ~s~%" k v)) input-map))

(defun make-adj-matrix (relations &optional ordered-vertices)
  (let* ((vertices (if ordered-vertices
		       ordered-vertices
		       (sort (loop for k being the hash-keys of relations collect k)
			     #'string<)))
	 (adj-matrix (make-array (list (length vertices) (length vertices)) :initial-element 0)))

    (maphash (lambda (k values)
	       (let ((i (position k vertices :test #'equal)))
		 (loop for v in values do
		   (let ((j (position v vertices :test #'equal)))
		     (setf (aref adj-matrix i j) 1)
		     (setf (aref adj-matrix j i) 1)))))
	     relations)
    (list adj-matrix vertices)))


;; (defun compute-distance (relations s)
;;   (let ((distances (make-hash-table :test #'equal))
;; 	(active (gethash s relations))
;; 	(next-gen '())
;; 	burnt)

;;     (loop for dist from 1 while (< 0 (length active)) do
;;       (loop for v in active if (not (member v burnt :test #'equal)) do
;; 	(setf (gethash (cons s v) distances) dist)
;; 	(setf next-gen (union next-gen (gethash v relations) :test #'equal))
;; 	(push v burnt))
;;       (setf active next-gen)
;;       (setf next-gen 'nil))
;;     (print-map distances)
;;     distances))


(defun sort-distances (distance-map)
  (let ((distance-list (loop for k being the hash-keys of distance-map
			     collect (list k (gethash k distance-map)))))
    (sort distance-list (lambda (a b)
			  (< (cadr a) (cadr b))))))


(defun compute-distances (relations)
  (let ((vertices (remove-duplicates
		   (loop for k being the hash-keys of relations collect k)
		   :test #'equal))
	(distances (make-hash-table :test #'equal)))

    (loop for v1 in vertices for i from 0 do
      (loop for j from 0 to i do
	(if (= i j)
	    (setf (gethash (list v1 v1) distances) 0)
	    (let* ((v2 (nth j vertices))
		   (active (gethash v1 relations))
		   next-gen
		   burnt)

	      (loop named outer for dist from 1 while (< 0 (length active)) do
		(loop for v in active if (not (member v burnt :test #'equal)) do
		  (push v burnt)
		  (when (equal v v2)
		    (setf (gethash (list v1 v2) distances) dist)
		    (setf (gethash (list v2 v1) distances) dist)
		    (return-from outer))
		  (setf next-gen (union next-gen (gethash v relations) :test #'equal)))
		(setf active next-gen)
		(setf next-gen 'nil))))))
    distances))


(defun random-choice (input-list) 
  (nth (random (length input-list)) input-list))

(defun substringp (needle haystack)
  (search (string needle) (string haystack) :test #'char=))


(let* ((input-fp "./input.txt")
       (relations (read-input input-fp))
       (distances (compute-distances relations))
       ;; (adj-matrix-and-dict (make-adj-matrix relations vertices))
       ;; (num-cuts 100000)
       ;; partition
       vertices
       adj-matrix-and-dict
       )


  (print-map distances)

  (setf vertices (nreverse (sort-distances distances)))
  (format t "Vertices: ~s~%" vertices)
  (let ((ref (car (car (car vertices)))))
    (setf vertices (sort
		    (loop for vertex in
				     (remove-if-not (lambda (x) (equal ref (car (car x)))) vertices)
			  collect (cadr (car vertex)))
		    (lambda (a b) (< (gethash (list ref a) distances)
				     (gethash (list ref b) distances))))))

  (format t "Vertices: ~s~%" vertices)
  (setf adj-matrix-and-dict (make-adj-matrix relations vertices))
  (format t "~s~%" (car adj-matrix-and-dict))
  
  )
