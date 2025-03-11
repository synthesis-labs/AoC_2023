(defun split-string (input-str split-char &optional (accum '()))
  (let ((split-at (position split-char input-str)))
    (if (null split-at)
	(progn (push input-str accum)
	       (nreverse accum))
	(let ((before (subseq input-str 0 split-at))
	      (after (subseq input-str (1+ split-at))))
	  (push before accum)
	  (split-string after split-char accum)))))


(defun calculate-hash (input-string)
  (let ((current 0))
    (loop for char_ across input-string do
      (setf current (mod (* 17 (+ current (char-code char_))) 256)))
  current))


(defun interpret-instruction (input-string)
  (let* ((len (length input-string))
	 (last-char (char input-string (1- len))))
    (if (eql last-char #\-)
	(let ((label (subseq input-string 0 (1- len))))
	  (list (calculate-hash label) label 'nil))
	(let ((label (subseq input-string 0 (- len 2)))
	      (focal-length (digit-char-p (char input-string (1- len)))))
	  (list (calculate-hash label) label focal-length)))))


(defun stack-lenses (input-string &optional (number-boxes 256))
  (let* ((instruction-strings (split-string input-string #\,))
	 (box-set (make-array (list number-boxes) :initial-element '() :adjustable 'nil)))

    (loop for instruction in instruction-strings do
      (destructuring-bind (box-id label focal-length) (interpret-instruction instruction)
	(let* ((box (aref box-set box-id))
	       (element (list label focal-length))
	       (target-pos (position-if (lambda (x) (equal label (car x))) box)))
	  (cond
	    ((and (null focal-length) (null target-pos))) ;; do nothing
	    ((null focal-length) (setf (nth target-pos box) 'nil))
	    ((null target-pos) (push element (aref box-set box-id)))
	    ('T (setf (nth target-pos box) element))))))

    (loop for i from 0 for b across box-set do
      (setf (aref box-set i) (nreverse (remove 'nil b))))
    box-set))


(defun compute-focal-power (box-set)
  (let ((total 0))
    (loop for box-id from 1 for box across box-set do
      (loop for slot-id from 1 for lens in box do
	(setf total (+ total (* box-id slot-id (cadr lens))))))
    total))


(let ((result 0))
  (with-open-file (stream "./input.txt")
    (setf result (compute-focal-power (stack-lenses (read-line stream 'nil)))))
  (format t "Focal power: ~s~%" result))

;; 511215 is the right answer for part 1
;; 236057 is the right answer for part 2
