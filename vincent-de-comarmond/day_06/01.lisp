(defun parse-line (line &optional accum)
  (let* ((start (position-if #'digit-char-p line))
	 (sub (subseq line start))
	 (end (position #\Space sub)))

    (if (null end) (push (parse-integer sub) accum)
	(progn
	  (push (parse-integer (subseq sub 0 end)) accum)
	  (parse-line (subseq sub end) accum)))))

(defun quadratic (a b c)
  (let ((disc (sqrt (- (* b b) (* 4 a c)))))
    (cons (/ (- 0 b disc) (* 2 a)) (/ (- disc b) (* 2 a)))))


(defun count-ways (time-dist)
  (destructuring-bind (time . dist) time-dist
    ;; x*(time-x) > dist => 0 > x^2 -time.x + dist => x0 <  or x1 < 0
    (destructuring-bind (q0 . q1) (quadratic 1 (- time) dist)
      (1+ (- (floor q1) (ceiling q0)
	     (if (= (floor q1) q1) 1 0)
	     (if (= (ceiling q0) q0) 1 0))))))


(let ((input-fp "./input.txt")
      (lines '()))

  (with-open-file (stream input-fp)
    (loop for line = (read-line stream 'nil) while line do
      (push (parse-line line) lines)))

  (setf lines (loop for idx from 0 to (1- (length (car lines)))
		    collect (cons (nth idx (cadr lines)) (nth idx (car lines)))))

  (format t "Solution: ~s~%" (apply '* (mapcar 'count-ways lines))))

;; 449550 is the right answer for part 1
