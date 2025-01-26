(with-open-file (stream "./input.txt")
  (let ((total 0))
    (loop for line = (read-line stream nil) while line do 
      (setf total
	    (+ total (parse-integer (coerce (list
					     (find-if 'digit-char-p line)
					     (find-if 'digit-char-p line :from-end t)) 'string)))))
    (format t "Part 1 solution: ~s~%" total)))

;; 54239 is the solution for part 1
