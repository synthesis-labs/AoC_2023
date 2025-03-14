;; (defun print-grid (grid max-idx cols)
;;   (loop for idx from 0 to max-idx do
;;     (if (and (> idx 0) (= (mod (1+ idx) cols) 0))
;; 	(format t "~c~%" (gethash idx grid #\.))
;; 	(format t "~c" (gethash idx grid #\.)))))


(defun read-grid (&key input-fp (size 15000))
  (let ((grid (make-hash-table :size size))
	(cols 0)
	(max-idx 0))
    (with-open-file (stream input-fp)
      (loop for idx from 0 for char_ = (read-char stream 'nil)
	    while char_ do
	      (case char_
		(#\newline (if (= 0 cols) (setf cols max-idx)))
		(#\. (incf max-idx))
		(otherwise
		 (setf (gethash max-idx grid) char_)
		 (incf max-idx)))))
    (list grid cols max-idx)))


(defun down-multiple (number mod-val)
  (- number (mod number mod-val)))

(defun up-multiple (number mod-val)
  (+ (down-multiple number mod-val) mod-val))

(defun trace-ray (grid start direction cols max-idx)
  (let ((ray (list (list start 'nil)))
	(found-end 0)
	(finished 'nil)
	(stop (cond ((= direction 1) (1- (up-multiple start cols)))
		    ((= direction -1) (down-multiple start cols))
		    ((< direction 0) (mod start cols))
		    ((> direction 0) (+ (- max-idx cols) (mod start cols))))))
    (do ((idx start)) (finished)
      (setf found-end idx)
      (let ((found-char (gethash idx grid))
	    (deflects 'nil))
	(if (or (eql found-char #\\) (eql found-char #\/)
		(and (eql found-char #\-) (> (abs direction) 1))
		(and (eql found-char #\|) (= (abs direction) 1)))
	    (setf deflects 'T))
	
	(when (and deflects (not (= idx start)))
	  (setf finished 'T)
	  (setf ray (push (list idx (gethash idx grid)) ray))))
      (if (or (and (> direction 0) (>= idx stop))
	      (and (< direction 0) (<= idx stop)))
	  (setf finished 'T))
      (setf idx (+ idx direction)))

    (if (= 2 (length ray))
	(nreverse ray)
	(nreverse (push (list found-end 'nil) ray)))))


(defun redirect (direction reflector cols)
  ;; (format t "Direction: ~s~%Reflector: ~s~%Cols: ~s~%" direction reflector cols)
  (if (> direction 0)
      (case reflector
	(#\\ (if (= direction 1) (list cols) '(1)))
	(#\/ (if (= direction 1) (list (- cols)) '(-1)))
	(#\- '(-1 1))
	(#\| (list (- cols) cols)))
      (case reflector
	(#\\ (if (= direction -1) (list (- cols)) '(-1)))
	(#\/ (if (= direction -1) (list cols) '(1)))
	(#\- '(-1 1))
	(#\| (list (- cols) cols)))))


(defun solve-line-segments (grid cols max-idx)
  (let ((active '((0 1)))
	(line-segments '()))

    (loop while (> (length active) 0) do
      ;; (format t "Active: ~s~%" active)
      ;; (format t "Active length: ~s~%" (length active))
      (destructuring-bind (start dir) (pop active)
	;; (format t "Unbundled start:~s,~cdirection: ~s~%" start #\tab dir)
	(let ((ray (trace-ray grid start dir cols max-idx)))
	  ;; (format t "Traced Ray~%")
	  (when (not (member ray line-segments :test 'equal))
	    (push ray line-segments)
	    ;; (format t "Line-Segments length: ~s~%" (length line-segments))
	    ;; (format t "~s~%" ray)
	    (destructuring-bind (end reflector) (cadr ray)
	      (loop for reflection in (redirect dir reflector cols) do
		;; (format t "Result: ~s~%" reflection)
		;; (format t "Reflection: ~s~%" reflection)
		(push (list end reflection) active)))))))
    line-segments))


(defun determine-energized (line-segments cols)
  (let ((line-grid-ids (mapcar (lambda (seg) (mapcar 'car seg)) line-segments))
	(grid-ids '()))
    ;; (format t "Line Grid ids: ~s~%" (reverse line-grid-ids))
    (setf line-grid-ids (mapcar (lambda (x) (sort x #'<)) line-grid-ids))

    (loop for segment in line-grid-ids do
      (destructuring-bind (start end) segment
	(loop for idx from start to end by (if (>= (- end start) cols)
					       cols
					       1)
	      do (push idx grid-ids))))
    (sort (remove-duplicates grid-ids) #'<)))


(destructuring-bind (grid cols max-idx) (read-grid :input-fp "./sample.txt")
  ;;   (format t "~s~%" cols)
  ;;   (format t "~s~%" rows)
  ;; (print-grid grid max-idx cols)
  
  ;; (print (trace-ray grid 35 10 cols max-idx))
  ;; (print (trace-ray grid 5 10 cols max-idx))
  (let ((energized-squares '())
	(grid-ids '())
	;; (grid-copy (make-hash-table))
	)
    (setf energized-squares (solve-line-segments grid cols max-idx))
    (setf grid-ids (determine-energized energized-squares cols))
    ;; (format t "~s~%" energized-squares)
    ;; (format t "~s~%" grid-ids)
    (format t "Number energized squares: ~s~%" (length grid-ids))
    ;; (loop for id in grid-ids do (setf (gethash id grid-copy) #\#))
    ;; (print-grid grid-copy max-idx cols)
    ;; (format t "~%")
    ;; (print-grid grid max-idx cols)
    
    ;; (loop for item in grid-ids do
    ;; ;; (format t "Min item: ~s~%" (apply 'min item))
    ;; ;; (format t "Max item: ~s~%~%" (apply 'max item))
    ;;   (loop for idx from (apply 'min item) to (apply 'max item) do
    ;; 	    (setf total (adjoin idx total)))

    ;; 	  )
    ;; ;; (format t "Length squares: ~s~%" (length energized-squares))
    ;; ;; (format t "~s~%" grid-ids)
    ;; (format t "~s~%" (length total))
    ;; (format t "~s~%" (sort total #'<))
    )

  ;; (format t "Trace Ray: ~s~%" (trace-ray grid 87 10 cols max-idx))
  )

;; 7183 is too low for part 1
