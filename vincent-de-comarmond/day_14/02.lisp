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


(defun spin (columns)
  (let* ((north-columns (mapcar 'roll columns))
	 (north-rows (transpose north-columns))
	 (west-rows (mapcar 'roll north-rows))
	 (west-columns (transpose west-rows))
	 (south-columns (mapcar 'reverse
				(mapcar 'roll
					(mapcar 'reverse west-columns))))
	 (south-rows (transpose south-columns))
	 (east-rows (mapcar 'reverse (mapcar 'roll (mapcar 'reverse south-rows)))))
    (transpose east-rows)))


(defun count-load (columns)
  (let ((num-rows (length (car columns)))
	(total 0))
    (loop for col in columns do
      (loop for row from 0 to (1- num-rows) do
	(if (eql (nth row col) #\O)
	    (setf total (+ total (- num-rows row))))))
    total))


(defun determine-periodicity (columns &key
					(warmup 500)
					(watch-length 500) 
					(target 1000000000))
  (let ((warmed columns)
	(key 'nil)
	(periodic-table (make-hash-table :test 'equal :size (* 2 watch-length)))
	(simplified-table (make-hash-table :size (* 2 watch-length))))
    (dotimes (n warmup)
      (if (= 0 (mod n 50)) (format t "Performing warmup cycle: ~s~%" n))
      (setf warmed (spin warmed)))

    (dotimes (n watch-length)
      (if (= 0 (mod n 50)) (format t "~cPerforming watch cycle: ~s~%" #\tab n))
      (setf key  warmed)

      (if (null (gethash key periodic-table))
	  (setf (gethash key periodic-table)
		(list (+ warmup n)))
	  (setf (gethash key periodic-table)
		(push (+ warmup n) (gethash key periodic-table))))

      (if (< n watch-length)
	  (setf warmed (spin warmed))))

    (maphash (lambda (k v)
	       (let ((new-key (apply 'max v))
		     (new-value (remove-duplicates (mapcar '- v (rest v)))))
		 (if (not (= 1 (length new-value)))
		     (error "Not enough warmup time. Not yet in stable state"))
		 (if (< (length v) 3)
		     (error "Not enough watch time. Not enough examples to be sure of period."))

		 (setf (gethash new-key simplified-table)
		       (cons new-value (count-load k)))))
	     periodic-table)

    (maphash (lambda (k v)
	       (let ((numerator (- target k))
		     (divisor (car (car v)))
		     (weight (cdr v)))
		 (if (= 0 (mod numerator divisor))
		     (return-from determine-periodicity weight))))
	     simplified-table)))


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

  (setf soln (determine-periodicity cols :warmup 150 :watch-length 100))
  (format t "Load after a huge number of cycles: ~s~%" soln))

;; 108889 is the right answer for part 1
;; 104671 is the right answer for part 2
