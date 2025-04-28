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
  (let (relations)
    (with-open-file (stream input-fp)
      (loop for idx from 0
	    for line = (read-line stream 'nil) while line do
	      (destructuring-bind (key values) (parse-line line)
		(loop for v in values if (not (equal v "")) do
		  (let ((alpha (if (string< key v) key v))
			(beta (if (string< key v) v key)))
		    (setf relations (adjoin (list alpha beta)
					    relations
					    :test #'equalp)))))))
    relations))


(defun random-choice (input-list) 
  (nth (random (length input-list)) input-list))


(defun priority-random-choice (input-list)
  (let ((degrees (make-hash-table :size (* 2 (length input-list)) :test #'equal)))
    (loop for (a b) in input-list do
      (incf (gethash a degrees 0))
      (incf (gethash b degrees 0)))

    (let (priority-list)
      (loop for (a b) in input-list do
	(loop repeat (+ (gethash a degrees) (gethash b degrees)) do
	  (push (list a b) priority-list)))
      (random-choice priority-list))))


(defun reduce-relations (relations merge-me)
  (destructuring-bind (a b) merge-me
    (let ((new-node (format nil "~a-~a" a b))
	  new-relations)
      (loop for edge in relations if (not (equalp edge merge-me)) do
	(destructuring-bind (c d) edge
	  (let ((new-edge (list (if (or (equal a c) (equal b c))
				    new-node c)
				(if (or (equal a d) (equal b d))
				    new-node d))))
	    (if (not (equal (car new-edge) (cadr new-edge)))
		(push new-edge new-relations)))))
      new-relations)))

(defun reduce-relations-loop (relations)
  (let ((copy (copy-tree relations))
	tmp)
    (loop while (< 1 (length (remove-duplicates copy :test #'equalp))) do
      (setf tmp (reduce-relations copy (priority-random-choice copy)))
      (setf copy tmp)
      (setf tmp 'nil))
    copy))


(defun substringp (needle haystack)
  (search (string needle) (string haystack) :test #'char=))


(defun find-cuts (relations string-set-a string-set-b)
  (let (cuts)
    (loop for (a b) in relations do
      (if (or (and (substringp a string-set-a) (substringp b string-set-b))
	      (and (substringp b string-set-a) (substringp a string-set-b)))
	  (push (list a b) cuts)))
    cuts))


(let* ((input-fp "./input.txt")
       (target 3)
       (relations (read-input input-fp))
       (num-cuts 100000)
       partition)

  (loop while (/= target num-cuts) for i from 1 do
    (format t "Invocations: ~s~%" i)

    (let* ((reduced (reduce-relations-loop relations))
	   (cuts (find-cuts relations
			    (first (car reduced))
			    (second(car reduced)))))

      (setf num-cuts (length cuts))
      (format t "Cuts: ~s~%" cuts)
      (if (null cuts)
	  (format t "~s~%" reduced))

      (if (= target num-cuts)
	  (setf partition (car reduced)))
      ))

  (format t "Partition power: ~s~%" (* (/ (1+ (length (car partition))) 4)
				       (/ (1+ (length (cadr partition))) 4))))
