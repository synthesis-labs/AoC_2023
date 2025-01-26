(ql:quickload "str")

(defun loc (index input-list)
  (if (> index 0) (nth index input-list)))

(defun str-list-to-int (input-list)
  (mapcar 'parse-integer (mapcar 'str:trim (str:split #\Space input-list))))

(defun read-num-line (input-str)
  (str-list-to-int (str:trim (car (last (str:split #\: input-str))))))

(defun pos-sort (pos) (lambda (a b) (< (nth pos a) (nth pos b))))

(defun make-seed-ranges (seed-list &optional (accum '()))
  (if (null seed-list) (sort accum (pos-sort 0))
      (progn (push (list (car seed-list) (+ (car seed-list) (cadr seed-list))) accum)
	     (make-seed-ranges (rest (rest seed-list)) accum))))

(defun map-seed (seed level)
  (if (= 0 (length level)) seed
      (destructuring-bind (start end delta) (car level)
	(if (and (<= start seed) (<= seed end))
	    (+ seed delta)
	    (map-seed seed (rest level))))))

(defun make-break-points (start end level)
  (let* ((break-points)
	 ;; Remove things with no overlap
	 (lvl-copy (remove-if-not (lambda (x) (and (<= start (cadr x)) (<= (car x) end))) level))
	 ;; Truncate endpoints if start-end cut's them
	 (lvl-copy (mapcar (lambda (x) (list (max start (car x)) (min end (cadr x)))) lvl-copy)))

    (if (null lvl-copy) (progn (push end break-points)
			       (push start break-points))

	(let ((lowest (car (car lvl-copy)))
	      (highest (cadr (car (last lvl-copy))))
	      (max-idx (1- (length lvl-copy))))
	  (loop for idx from 0 for el in lvl-copy do
	    (let* ((previous (loc (1- idx) lvl-copy))
		   (p-cadr (if previous (cadr previous)))
		   (extend-l (and (= idx 0) (< start lowest)))
		   (extend-h (and (= idx max-idx) (< highest end)))
		   (extend-j (if p-cadr (< (1+ p-cadr) (car el)))))
	      (if extend-l (progn (push start break-points)
				  (push (1- lowest) break-points)))
	      (if extend-h (progn (push (1+ highest) break-points)
				  (push end break-points)))
	      (if extend-j (progn (push (1+ p-cadr) break-points)
				  (push (1- (car el)) break-points)))

	      (push (car el) break-points)
	      (push (cadr el) break-points)))))
    (sort break-points #'<)))

(defun map-seed-range (start end level)
  (let ((break-points (make-break-points start end level)))
    (setf break-points (mapcar (lambda (seed) (map-seed seed level)) break-points))
    (loop for (s e) on break-points by #'cddr collect (list s e))))

(let ((input-fp "./input.txt")
      (seeds '())
      (levels '())
      (cache '()))
  
  (with-open-file (stream input-fp)
    (loop for idx from 0 for line = (read-line stream 'nil)
	  while line do
	    (cond ((= 0 idx) (setf seeds (make-seed-ranges (read-num-line line))))
		  ((and (= 0 (length line)) (> (length cache) 0))
		   (progn (push (sort cache (pos-sort 0)) levels)
			  (setf cache '())))
		  ((or (= 0 (length line)) (find #\: line))) ;; skip
		  (t (destructuring-bind (dst src range) (read-num-line line)
		       (push (list src (1- (+ src range)) (- dst src)) cache))))))

  (push (sort cache (pos-sort 0)) levels)
  (setf levels (nreverse levels))
  
  (loop for level in levels do
    (setf seeds (loop for s-pair in seeds
		      nconcing (map-seed-range (car s-pair) (cadr s-pair) level))))
  (print (apply 'min (mapcar 'car seeds))))

;; 1240035 is the right answer
