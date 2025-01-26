(defun find-low (input-str)
  (let* ((location (position-if 'digit-char-p input-str))
	 (value (if location (digit-char-p (aref input-str location)) 0)))
    (loop for val from 1
	  for word in '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine") do
	    (let ((word-loc (search word input-str)))
	      (if (or (and word-loc (null location))
		      (and word-loc location (< word-loc location)))
		  (progn
		    (setf location word-loc)
		    (setf value val)))))
    value))

(defun find-high (input-str)
  (let* ((location (position-if 'digit-char-p input-str :from-end t))
	 (value (if location (digit-char-p (aref input-str location)) 0)))
    (loop for val from 1
	  for word in '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine") do
	    (let ((word-loc (search word input-str :from-end t)))
	      (if (or (and word-loc (null location))
		      (and word-loc location (> word-loc location)))
		  (progn
		    (setf location word-loc)
		    (setf value val)))))
    value))

(with-open-file (stream "./input.txt")
  (let ((total 0))
    (loop for line = (read-line stream nil) while line do
      (let* ((first-digit (find-low line))
	     (last-digit (find-high line)))
	(setf total (+ total (* 10 first-digit) last-digit))))
    (format t "Part 2 solution: ~s~%" total)))

;; 55343 is the correct solution for part 2
