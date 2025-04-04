(defun split (input-string input-char &optional (accum '()))
  (let ((pos (position input-char input-string)))
    (cond ((= 0 (length input-string)) (nreverse (push "" accum)))
	  ((null pos) (nreverse (push input-string accum)))
	  ('T (split (subseq input-string (1+ pos))
		     input-char
		     (push (subseq input-string 0 pos) accum))))))


(defun read-input (input-fp)
  (let ((pre-network '()))

    (with-open-file (stream input-fp)
      (let ((segments '()))

	(loop for line = (read-line stream 'nil) while line do
	  (setf segments (split (delete #\, line) #\space))
	  (let* ((node-name (subseq (car segments) 1))
		 (destinations (subseq segments 2))
		 (node (list :name node-name :destinations destinations)))

	    (case (char (car segments) 0)
	      (#\b
	       (setf (getf node :name) "broadcaster")
	       (setf (getf node :type) #\b))
	      (#\% (setf (getf node :type) #\f))
	      (#\& (setf (getf node :type) #\c)))
	    (push node pre-network)))))
    pre-network))


(defun make-flip-flop (destinations)
  (let ((state 0))
    #'(lambda (source-name x)
	(declare (ignore source-name))
	(when (= x 0)
	  (setf state (- 1 state))
	  (list state destinations)))))


(defun make-conjunction (inputs destinations)
  (let ((memory (make-hash-table :size 10 :test #'equal)))
    (loop for input in inputs do
      (setf (gethash input memory) 0))
    #'(lambda (source-name x)
	(setf (gethash source-name memory) x)
	(if (member 0 (loop for v being the hash-values of memory
			    collect v))
	    (list 1 destinations)
	    (list 0 destinations)))))


(defun make-broadcaster (destinations)
  #'(lambda (source-name x)
      (declare (ignore source-name))
      (list x destinations)))


(defun make-network (pre-network)
  (let ((conjunctions (loop for node in pre-network if (eql #\c (getf node :type))
			    collect (getf node :name)))
	(conjunction-inputs (make-hash-table :size 10 :test #'equal))
	(network (make-hash-table :size 200 :test #'equal)))

    (loop for conjunction in conjunctions do
      (setf (gethash conjunction conjunction-inputs) '()))

    (loop for node-info in pre-network do
      (destructuring-bind (&key name destinations &allow-other-keys) node-info
	(loop for conjunction in conjunctions do
	  (if (member conjunction destinations :test #'equal)
	      (push name (gethash conjunction conjunction-inputs))))))

    (loop for node-info in pre-network do
      (destructuring-bind (&key name type destinations) node-info
	(case type
	  (#\b (setf (gethash "broadcaster" network) (make-broadcaster destinations)))
	  (#\c (setf (gethash name network)
		     (make-conjunction (gethash name conjunction-inputs)
				       destinations)))
	  (#\f (setf (gethash name network) (make-flip-flop destinations))))))
    network))


(defun push-button (network)
  (let ((low-pulses 0)
	(high-pulses 0)
	(active (list (list "button" 0 (list "broadcaster" ))))
	(next-gen '()))

    (loop while (> (length active) 0) do
      (loop for current in (reverse active) do
	(destructuring-bind (source-name sig destinations) current
	  (loop for dest in destinations do
	    (case sig
	      (0 (incf low-pulses))
	      (1 (incf high-pulses)))
	    (let* ((func (gethash dest network))
		   (outputs (if func (funcall func source-name sig))))
	      ;; (format t "~s -> ~s -> ~s ~s~%" source-name sig dest (list low-pulses high-pulses))
	      (if outputs
		  (push (list dest (car outputs) (cadr outputs)) next-gen))))))
      (setf active next-gen)
      (setf next-gen '()))
    (list low-pulses high-pulses)))


(time (let* ((input-fp "./input.txt")
	     (pre-network (read-input input-fp))
	     (network (make-network pre-network))
	     (pulses (loop repeat 1000 collect (push-button network)))
	     (total (* (apply #'+ (mapcar #'car pulses))
		       (apply #'+ (mapcar #'cadr pulses)))))

	(format t "Total: ~s~%" total)))

;; 886347020 is the right answer for part 1
;; Run time is ... like 0.01 s - looks to be mostly compilation time. Rerunning is ... 0 s
