(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))


(defun parse-line (line)
  (destructuring-bind (key values) (split-str line #\:)
    (list key (split-str values #\ ))))


(defun read-input (input-fp)
  (let (relationships)
    (with-open-file (stream input-fp)
      (loop for idx from 0
	    for line = (read-line stream 'nil) while line do
	      (destructuring-bind (key values) (parse-line line)
		(loop for v in values if (not (equal v "")) do
		  (setf relationships (adjoin (sort (list key v) #'string<)
					      relationships
					      :test #'equal))))))
    relationships))


(defun print-map (input-map)
  (maphash (lambda (k v) (format t "~a: ~a~%" k v)) input-map))


(defun to-dot (relations output-fp)
  (with-open-file (flout output-fp :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
    (write-line "graph G {" flout)
    (write-line "rankdir=\"LR\";" flout)
    (loop for (k v) in relations do
      (write-line (format 'nil "~c~a -- ~a" #\tab k v) flout))
    (write-line "}" flout)))


(defun remove-relations (relations remove-me)
  (let ((sorted (mapcar (lambda (x) (sort x #'string<)) remove-me)))
   (set-difference relations sorted :test #'equal)))


(defun make-partitions (relations)
  (let ((copy (copy-seq relations))
	current
	partitions
	size)

    (loop while (< 0 (length copy)) do
      (if (null current) (setf current (pop copy)))

      (setf size (length current))
      (loop for k in copy if (intersection current k :test #'equal)
	    do (setf current (union current k :test #'equal)))

      (when (= size (length current))
	(setf copy (loop for k in copy if (not (intersection current k :test #'equal))
			 collect k))
	(push current partitions)
	(setf current 'nil)))

    partitions))


;;;; This worked okay for part 1 ... it was small enough to brute-force
(defun solve (relations)
  (let* ((num-relns (length relations))
	 (worst-case (* num-relns (1- num-relns) (- num-relns 2)))
	 (count 0)
	 partitions)

    (loop named outer for i in relations do
      (loop for j in relations if (not (equal i j)) do
	(loop for k in relations if (not (or (equal i k) (equal j k)))
	      do (incf count)
		 (when (= 0 (mod count 500))
		   (format t "Computing iteration ~s of ~s~%" count worst-case))
		 (setf partitions (make-partitions
				   (remove-relations relations (list i j k))))
		 (if (= 2 (length partitions))
		     (return-from outer)))))

    (list :partitions partitions :score (apply '* (mapcar 'length partitions)))))


(defun find-edges (relations vertices-to-find)
  (let ((vertex-edges (make-hash-table :test #'equal)))
    (loop for vertices in relations do
      (if (member (car vertices) vertices-to-find :test #'equal)
	  (push vertices (gethash (car vertices) vertex-edges '())))
      (if (member (cadr vertices) vertices-to-find :test #'equal)
	  (push vertices (gethash (cadr vertices) vertex-edges '()))))
    vertex-edges))


(let* ((input-fp "./input.txt")
       (relations (read-input input-fp)))

  (to-dot relations "full.dot")
  ;;;; visual inspection suggests we investigate rkh kpc and hrs

  (print-map (find-edges relations '("rkh" "kpc" "hrs")))
  ;; suggestions are
  ;; rkh - gnb, rhc, mhs, xjc, sph
  ;; kpc - nnl, nkl, cct, crj, rcc
  ;; hrs - nhx, jqq, crp, xbl, mnf

  ;; further inspection suggests
  ;; rkh sph
  ;; kpc nnl
  ;; hrs mnf

  (let ((partitions (make-partitions (remove-relations relations '(("rkh" "sph")
								   ("kpc" "nnl")
								   ("hrs" "mnf"))))))
    (format t "Number partitions: ~s~%" (length partitions))
    (format t "Partitions power: ~s~%" (* (length (car partitions)) (length (cadr partitions))))))

;; 614655 is the right answer for part 1
