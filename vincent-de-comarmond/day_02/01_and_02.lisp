(ql:quickload "str")

(defun parse-line (input-line)
  (let* ((res-table (make-hash-table :test 'equal)))
    (loop for game in (mapcar 'str:trim (str:split #\; (second (str:split #\: input-line)))) do
      (loop for val_col in (str:split #\, game) do
	(destructuring-bind (val col) (str:split #\Space (str:trim val_col))
	  (if (null (gethash col res-table))
	      (setf (gethash col res-table) (parse-integer val))
	      (setf (gethash col res-table) (max (parse-integer val) (gethash col res-table)))))))
    res-table))

;;;; Part 1
(let ((input "./input.txt")
      (limits (make-hash-table :test 'equal))
      (total 0))
  (setf (gethash "red" limits) 12)
  (setf (gethash "green" limits) 13)
  (setf (gethash "blue" limits) 14)
  (with-open-file (stream input)
    (loop for idx from 1
	  for line = (read-line stream 'nil)
	  while line do
	    (let ((legit '())
		  (parsed (parse-line line)))
	      (maphash (lambda (k v) (push (<= v (gethash k limits 0)) legit)) parsed)
	      (if (not (member 'nil legit)) (setf total (+ total idx))))))
  (print total))


;;;; Part 2
(let ((input "./input.txt")
      (total 0))
  (with-open-file (stream input)
		  (loop for idx from 1
			for line = (read-line stream 'nil)
			while line do
			(let ((tmp 1)
			      (parsed (parse-line line)))
			  (loop for v being the hash-value of parsed do (setf tmp (* tmp v)))
			  (setf total (+ total tmp)))))
  (print total))
