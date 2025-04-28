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
		    (setf relations (adjoin (list (list alpha) (list beta))
					    relations
					    :test #'equalp)))))))
    relations))


(defun random-choice (input-list) 
  (nth (random (length input-list)) input-list))


;;;; Super inefficient - you can definitely do better now that
;;;; you have a seemingly working algorithm to use
(defun reduce-relations (relations merge-me)
  (let ((merged (sort (union (car merge-me) (cadr merge-me) :test #'equalp)
		      #'string<))
	new-relns)

    ;;; Note you've screwed this up a bit. It's better to
    ;; destructure merge-me and use the original's to test
    (loop for edge in relations if (not (equalp edge merge-me)) do
      (let (new-el)
	(destructuring-bind (a b) edge
	  (if (intersection merged b :test #'equalp)
	      (push merged new-el)
	      (push b new-el))
	  (if (intersection a merged :test #'equalp)
	      (push merged new-el)
	      (push a new-el))
	  (if (set-exclusive-or (car new-el) (cadr new-el))
	      (push new-el new-relns)))))
    new-relns))

(defun reduce-relations-loop (relations)
  (let ((copy (copy-tree relations))
	tmp)
    
    (loop while (< 1 (length (remove-duplicates copy :test #'equalp)))
	  for i from 1 do
	    (setf tmp (reduce-relations copy (random-choice copy)))
	    (when (= 0 (mod i 100))
	      (format t "~cinvocations: ~s~%" #\tab i)
	      (format t "~cnumber-edges: ~s~%" #\tab (length tmp)))

	    (setf copy tmp)
	    (setf tmp 'nil))
    copy))


(defun find-cuts (relations subset-a subset-b)
  (let (cuts)
    (loop for edge in relations do
      (destructuring-bind (a b) edge
	(let ((a-a (intersection subset-a a :test #'equalp))
	      (a-b (intersection subset-a b :test #'equalp))
	      (b-a (intersection subset-b a :test #'equalp))
	      (b-b (intersection subset-b b :test #'equalp)))
	  (if (or (and a-a b-b)
		  (and a-b b-a))
	      (push edge cuts)))))
    cuts))


(let* ((input-fp "./input.txt")
       (target 3)
       (relations (read-input input-fp))
       (num-cuts 100000)
       partition)

  (format t "~cinvocations: ~s~%" #\tab 0)
  (format t "~cnumber-edges: ~s~%" #\tab (length relations))
  (loop while (/= target num-cuts) for i from 1 do
    (format t "Invocations: ~s~%" i)

    (let* ((reduced (reduce-relations-loop relations))
	   (cuts (find-cuts relations
			    (first (car reduced))
			    (second(car reduced)))))

      (setf num-cuts (length cuts))
      (format t "Minimum cut found: ~s~%" num-cuts)

      (if (= target num-cuts)
	  (setf partition (car reduced)))))
  
  (format t "Partition power: ~s~%" (* (length (car partition))
				       (length (cadr partition)))))
