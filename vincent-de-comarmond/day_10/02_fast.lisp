(defparameter *width* 'nil)
(defparameter *s-idx* 'nil)


(defun read-input-map (input-fp)
  (let ((mouse-map (make-hash-table :size 20000))
	(idx 0))
    (with-open-file (stream input-fp)
      (loop for ch = (read-char stream 'nil) while ch do
	(if (eql ch #\Newline)
	    (if (null *width*) (defparameter *width* idx))
	    (progn (setf (gethash idx mouse-map) ch)
		   (if (eql ch #\S) (defparameter *s-idx* idx))
		   (incf idx)))))
    mouse-map))


(defun path-neighbour (pos prev-pos map)
  (let ((val (gethash pos map))
	(n (- pos *width*))
	(e (1+ pos))
	(s (+ pos *width*))
	(w (1- pos)))
    (case val
      (#\S (cond
	     ((find (gethash n map) "|F7") n)
	     ((find (gethash e map) "-J7") e)
	     ((find (gethash s map) "|JL") s)
	     ((find (gethash w map) "-LF") w)))
      (#\| (if (= prev-pos s) n s))
      (#\F (if (= prev-pos e) s e))
      (#\- (if (= prev-pos w) e w))
      (#\7 (if (= prev-pos w) s w))
      (#\J (if (= prev-pos n) w n))
      (#\L (if (= prev-pos e) n e)))))


(defun walk-map (input-hash-tab route)
  (let* ((curr (car route))
	 (prev (cadr route))
	 (neighbour (path-neighbour curr prev input-hash-tab)))
    (if (and (eql #\S (gethash neighbour input-hash-tab)) (> (length route) 1))
	route
	(walk-map input-hash-tab (push neighbour route)))))


(defun get-starts (route)
  (let ((dn (- *width*))
	(de  1)
	(ds *width*)
	(dw -1)
	(route-len (length route))
	(starts (make-hash-table :size 20000)))

    (loop for idx from 0 for point in route do
      (let* ((prev (nth (mod (1- idx) route-len) route))
	     (next (nth (mod (1+ idx) route-len) route))
	     (dprev (- point prev))
	     (dnext (- next point)))
	(cond ((= dprev dn)
	       (cond ((= dnext dn)
		      (setf (gethash (1- point) starts) 'T))
		     ((= dnext de)
		      (setf (gethash (1- point) starts) 'T)
		      (setf (gethash (- point *width* 1) starts) 'T)
		      (setf (gethash (- point *width*) starts) 'T))
		     ((= dnext dw)
		      (setf (gethash (+ point *width* -1) starts) 'T))))
	      ((= dprev de)
	       (cond ((= dnext dn)
		      (setf (gethash (- point *width* 1) starts) 'T))
		     ((= dnext de)
		      (setf (gethash (- point *width*) starts) 'T))
		     ((= dnext ds)
		      (setf (gethash (- point *width*) starts) 'T)
		      (setf (gethash (- (1+ point) *width*) starts) 'T)
		      (setf (gethash (1+ point) starts) 'T))))
	      ((= dprev ds)
	       (cond ((= dnext de)
		      (setf (gethash (- (1+ point) *width*) starts) 'T))
		     ((= dnext ds)
		      (setf (gethash (1+ point) starts) 'T))
		     ((= dnext dw)
		      (setf (gethash (1+ point) starts) 'T)
		      (setf (gethash (+ point *width* 1) starts) 'T)
		      (setf (gethash (+ point *width*) starts) 'T))))
	      ((= dprev dw)
	       (cond ((= dnext dn)
		      (setf (gethash (+ point *width*) starts) 'T)
		      (setf (gethash (+ (1- point) *width*) starts) 'T)
		      (setf (gethash (1- point) starts) 'T))
		     ((= dnext ds)
		      (setf (gethash (+ point *width* 1) starts) 'T))
		     ((= dnext dw)
		      (setf (gethash (+ point *width*) starts) 'T)))))))

    (loop for point in route do
      (remhash point starts))
    starts))


(defun nesw-neighbours (k burnt)
  (let ((n (- k *width*))
	(e (1+ k))
	(s (+ k *width*))
	(w (1- k))
	(neighbours '()))
    (if (and (null (gethash n burnt)) (> n 0))
	(push n neighbours))
    (if (and (null (gethash e burnt)) (= (ceiling e *width*) (ceiling k *width*)))
	(push e neighbours))
    (if (and (null (gethash s burnt)) (< s (* *width* *width*)))
	(push s neighbours))
    (if (and (null (gethash w burnt)) (= (floor w *width*) (floor k *width*)))
	(push w neighbours))
    neighbours))


(defun light-fires (route)
  (let* ((active (get-starts (reverse route)))
	 (burnt (make-hash-table :size 5000)))
    (loop for toast in route do (setf (gethash toast burnt) 'T))

    (loop while (> (hash-table-count active) 0) do
      (loop for k being the hash-key in active do
	(setf (gethash k burnt) 'T)
	(loop for ngh in (nesw-neighbours k burnt) do
	  (setf (gethash ngh active) 'T))
	(remhash k active)))
    burnt))


(let* ((input-fp "./input.txt")
       (map (read-input-map input-fp))
       (route (walk-map map (list *s-idx*)))
       (burnt (light-fires route)))
  
  (format t "Part 1 - furthest point: ~s~%" (/ (length route) 2))
  (format t "Number of nesting positions: ~s~%" (- (hash-table-count burnt) (length route))))


;; 7086 is the right answer for part 1
;; 5117 is too high for part 2
;; 5116 is too high
;; 313 is too low for part 2
;; 317 is the correct answer for part 2
