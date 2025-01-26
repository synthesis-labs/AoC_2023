(ql:quickload "str")

(defun str-to-list (input-str)
  (mapcar 'parse-integer
     (str:split #\Space (str:trim (car (last (str:split #\: input-str)))))))

(defun create-mapping (lists-of-ranges)
  (lambda (seed)
    (let ((mapped))
      (loop for mapping in lists-of-ranges do
	(destructuring-bind (dest-start src-start range) mapping
	  (if (and (null mapped) (<= src-start seed) (< seed (+ src-start range)))
	      (setf mapped (+ dest-start (- seed src-start))))))
      (if (null mapped) seed mapped))))

(let ((input-fp "./input.txt")
      (seeds '())
      (cache '()))

  (with-open-file (stream input-fp)
    (loop for idx from 0
	  for line = (read-line stream 'nil) while line do
	    (cond ((= 0 idx) (setf seeds (str-to-list line)))
		  ((= 0 (length line))) ;; skip
		  ((and (find #\: line) (< 0 (length cache)))
		   (setf seeds (mapcar  (create-mapping cache) seeds))
		   (setf cache '()))
		  ((null (find #\: line)) (push (str-to-list line) cache)))))
  (setf seeds (mapcar (create-mapping cache) seeds))
  (print (apply 'min seeds)))

;; 24964261 is not the right answer ... it is too low
;; 650599855 is the right answer
