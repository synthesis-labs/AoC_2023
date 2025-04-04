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


(defun cycles-to-conjunctor-feeding-rx (network &optional
						  (presses 0)
						  (memories (make-hash-table :size 4 :test #'equal)))
  (incf presses)
  (when (and (>= (length (gethash "qs" memories)) 2)
	     (>= (length (gethash "sv" memories)) 2)
	     (>= (length (gethash "pg" memories)) 2)
	     (>= (length (gethash "sp" memories)) 2))
    (maphash (lambda (k v)
	       (format t "~s: ~s~%" k v))
	     memories)
    (format t "Presses: ~s~%" presses)
    (return-from cycles-to-conjunctor-feeding-rx memories))
  
  (let ((rx-pulses-received 0)
	(rx-received-low-pulse 'nil)
	(active (list (list "button" 0 (list "broadcaster" ))))
	(next-gen '()))

    (loop while (> (length active) 0) do
      (loop for current in (reverse active) do
	(destructuring-bind (source-name sig destinations) current
	  (loop for dest in destinations do
	    (when (equal dest "rx")
	      (incf rx-pulses-received)
	      (if (= sig 0)
		  (format t "Received low pulse")
		  (setf rx-received-low-pulse 'T)))

	    (let* ((func (gethash dest network))
		   (outputs (if func (funcall func source-name sig))))

	      (when (and (member "gf" destinations :test #'equal)
			 (= sig 1))
		(if (gethash source-name memories)
		    (push presses (gethash source-name memories))
		    (setf (gethash source-name memories) (list presses))))

	      ;; (format t "~s -> ~s -> ~s ~s~%" source-name sig dest (list low-pulses high-pulses))
	      (if outputs
		  (push (list dest (car outputs) (cadr outputs)) next-gen))))))

      (setf active next-gen)
      (setf next-gen '()))

    (if (and rx-received-low-pulse (= rx-pulses-received 1))
	presses
	(cycles-to-conjunctor-feeding-rx network presses memories))))


(time (let* ((input-fp "./input.txt")
	     (pre-network (read-input input-fp))
	     (network (make-network pre-network))
	     (cycles (cycles-to-conjunctor-feeding-rx network))
	     (presses (apply #'lcm (mapcar (lambda (x) (- (car x) (cadr x)))
					   (loop for v being the hash-values of cycles
						 collect v)))))

	(format t "Pushes requred: ~s~%" presses)))

;; 886347020 is the right answer for part 1
;; Run time is ... like 0.01 s

;; 2165000000 is too low for part 2
;; 6630000000 and still going (still too low)
;; Calling it quits at 8345000000

;; Okay ... so just checked the internet how to do this ('cause I'm tired now)
;; 233283622908263 is the right answer for part 2
;; runtime is ~ 0.05 s
