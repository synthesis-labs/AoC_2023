(defun parse-line (line)
  (parse-integer (remove #\Space (subseq line (position-if #'digit-char-p line)))))

(defun quadratic (a b c)
  (let ((disc (sqrt (- (* b b) (* 4 a c)))))
    (cons (/ (- 0 b disc) (* 2 a)) (/ (- disc b) (* 2 a)))))

(defun test-func (x time dist)
  (> (* x (- time x)) dist))

(defun walk-down (root time dist)
  (let* ((root-val (test-func root time dist))
	 (left (1- root))
	 (left-val (test-func left time dist)))

    (if (and root-val (null left-val))
	root
	(if (and left-val (null root-val))
	    left
	    (walk-down left time dist)))))

(defun walk-up (root time dist)
  (let* ((root-val (test-func root time dist))
	 (right (1+ root))
	 (right-val (test-func right time dist)))

    (if (and root-val (null right-val))
	root
	(if (and right-val (null root-val))
	    right
	    (walk-up right time dist)))))

(defun count-ways (time dist)
  (destructuring-bind (q0 . q1) (quadratic 1 (- time) dist)
    (let ((q0 (ceiling q0))
	  (q1 (floor q1)))

      (if (test-func q0 time dist)
	  (setf q0 (walk-down q0 time dist))
	  (setf q0 (walk-up q0 time dist)))

      (if (test-func q1 time dist)
	  (setf q1 (walk-up q1 time dist))
	  (setf q1 (walk-down q1 time dist)))

      (1+ (- q1 q0)))))

(let ((input-fp "./input.txt")
      (time) (dist))

  (with-open-file (stream input-fp)
    (loop for line = (read-line stream 'nil) for idx from 0
	  while line do (if (= 0 idx)
			    (setf time (parse-line line))
			    (setf dist (parse-line line))))
  
    (format t "Solution: ~s~%" (count-ways time dist))))

;; 28360141 is too high for part 2
;; Okay ... what's happened here is that there's a numerical accuracy error
;; 28360140 is the right answer for part 2  (crying face ... FFS)
