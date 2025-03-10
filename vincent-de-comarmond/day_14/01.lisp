(defun transpose (rows-or-columns)
  (loop for idx from 0 to (1- (length (nth 0 rows-or-columns)))
	collect (mapcar (lambda (tmp) (nth idx tmp)) rows-or-columns)))

(defun roll (row-or-column)
  (let ((result (copy-seq row-or-column)))
    (loop for char_ in result for idx from 0 do
      (when (and (> idx 0) (eql char_ #\O))
	(loop for idx2 from idx downto 1 do
	  (let ((curr (nth idx2 result))
		(prev (nth (1- idx2) result)))
	    (when (and (eql prev #\.) (eql curr #\O))
	      (setf (nth idx2 result) prev)
	      (setf (nth (1- idx2) result) curr))))))
    result))

(defun count-load (columns)
  (let ((num-rows (length (car columns)))
	(total 0))
    (loop for col in columns do
      (loop for row from 0 to (1- num-rows) do
	(if (eql (nth row col) #\O)
	    (setf total (+ total (- num-rows row))))))
    total))

(let ((input-fp "./input.txt")
      (tmp '())
      (rows '())
      (cols '())
      (soln '()))

  (with-open-file (stream input-fp)
    (loop for character_ = (read-char stream 'nil)
	  while character_ do
	    (if (not (eql #\newline character_))
		(push character_ tmp)
		(progn (push (nreverse tmp) rows)
		       (setf tmp '())))))
  (setf rows (nreverse rows))
  (setf cols (transpose rows))

  (setf soln (mapcar 'roll cols))
  (format t "Total load: ~s~%" (count-load soln)))

;; 108889 is the right answer for part 1
