(defun calculate-hash (input-string &optional (accum '()))
  (let ((current 0))
    (loop for char_ across input-string do
      (if (eql char_ #\,) 
	  (progn (push current accum)
		 (setf current 0))
	  (setf current (mod (* 17 (+ current (char-code char_))) 256))))
    (push current accum))
  accum)

(let ((result 0))
  (with-open-file (stream "./input.txt")
    (setf result (apply '+ (calculate-hash (read-line stream 'nil)))))
  (format t "Hash: ~s~%" result))

;; 511215 is the right answer for part 1
