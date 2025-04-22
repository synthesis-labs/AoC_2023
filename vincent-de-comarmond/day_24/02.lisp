(defun split-str (input-str split-char &optional (accumulator '()))
  (let ((split-pos (position split-char input-str :from-end 'T)))
    (if (null split-pos)
	(push input-str accumulator)
	(split-str (subseq input-str 0 split-pos)
		   split-char
		   (push (subseq input-str (1+ split-pos)) accumulator)))))

(defun parse-line (line)
  (destructuring-bind (start end) (split-str line #\@)
    (destructuring-bind ((x y z) (dx dy dz))
	(mapcar (lambda (x) (mapcar 'parse-integer (split-str x #\,))) (list start end))

      (let ((ttg (if (< dz 0) (abs (/ z dz)))))
	(list :x x :y y :z z :dx dx :dy dy :dz dz :ttg ttg)))))


(defun read-input (input-fp)
  (with-open-file (stream input-fp)
    (loop for idx from 0
	  for line = (read-line stream 'nil)  while line
	  collect (parse-line line))))

(defun predict (projectile time)
  (destructuring-bind (&key x dx y dy z dz &allow-other-keys) projectile
    (list :x (+ (* dx time) x) :y (+ (* dy time) y) :z (+ (* dz time) z))))


(defun future-collision (stone hail)
  (if (and (/= (getf stone :dx) (getf hail :dx))
	   (/= (getf stone :dy) (getf hail :dy))
	   (/= (getf stone :dz) (getf hail :dz)))

      (let ((tx (/ (- (getf hail :x) (getf stone :x))
		   (- (getf stone :dx) (getf hail :dx))))
	    (ty (/ (- (getf hail :y) (getf stone :y))
		   (- (getf stone :dy) (getf hail :dy))))
	    (tz (/ (- (getf hail :z) (getf stone :z))
		   (- (getf stone :dz) (getf hail :dz)))))
	(if (and (= tx ty tz)
		 (< 0 tx))
	    tx))))

;;;; Problem is ttg is huge in real problem
(defun ttg-sort (a b)
  (let ((ttg-a (getf a :ttg))
	(ttg-b (getf b :ttg)))
    (if (and ttg-a ttg-b)
	(< ttg-a ttg-b)
	(if (null ttg-a) 'nil 'T))))

(defun cross-product (hail1 hail2)
  (let ((dx1 (getf hail1 :dx))
	(dy1 (getf hail1 :dy))
	(dz1 (getf hail1 :dz)))
    (destructuring-bind (&key dx dy dz &allow-other-keys) hail2
      (list :x (- (* dy1 dz) (* dz1 dy))
	    :y (- (* dz1 dx) (* dx1 dz))
	    :z (- (* dx1 dy) (* dy1 dx))))))

(defun dot-product (hail1 hail2)
  (+ (* (getf hail1 :x) (getf hail2 :x))
     (* (getf hail1 :y) (getf hail2 :y))
     (* (getf hail1 :z) (getf hail2 :z))))

(defun traj-dist (proj1 proj2)
  (let* ((normal (cross-product proj1 proj2))
	 (mag (sqrt (dot-product normal normal)))
	 (diff (list :x (- (getf proj1 :x) (getf proj2 :x))
		     :y (- (getf proj1 :y) (getf proj2 :y))
		     :z (- (getf proj1 :z) (getf proj2 :z)))))
    (/ (dot-product normal diff) mag)))


;;;; Problem is ttg is huge in real problem
;; (defun generate-possibilities (hail)
;;   (let (possibilities)
;;     ;; Do not bother checking gcds here ... assume the problem is correctly set up
;;     (destructuring-bind (&key x dx y dy z dz ttg) hail
;;       (loop for t_ from 1 below ttg do
;; 	(push (list :dt t_
;; 		    :x (+ (* dx t_) x)
;; 		    :y (+ (* dy t_) y)
;; 		    :z (+ (* dz t_) z))
;; 	      possibilites)))
;;     possibilities))

(defun make-trajectory-sort (reference-trajectory)
  #'(lambda (a b)
      (let ((dist-a (traj-dist reference-trajectory a))
	    (dist-b (traj-dist reference-trajectory b)))
	(< dist-a dist-b))))


(defun make-inversion (hail1 hail2)
  (let ((x1 (getf hail1 :x))
	(x2 (getf hail2 :x))
	(y1 (getf hail1 :y))
	(y2 (getf hail2 :y))
	(z1 (getf hail1 :z))
	(z2 (getf hail2 :z))
	(dx1 (getf hail1 :dx))
	(dx2 (getf hail2 :dx))
	(dy1 (getf hail1 :dy))
	(dy2 (getf hail2 :dy))
	(dz1 (getf hail1 :dz))
	(dz2 (getf hail2 :dz)))

    #'(lambda (t dt)
	(let (


	      (dx3 (+ (/ (* (- dx2 dx1)t ) dt)
		      (/ (- x2 x1) dt)
		      dx2))
	      (dy3 (+ (/ (* (- dy2 dy1)t ) dt)
		      (/ (- y2 y1) dt)
		      dy2))
	      (dz3 (+ (/ (* (- dz2 dz1)t ) dt)
		      (/ (- z2 z1) dt)
		      dz2)))




	 )
   )))



(time (let* ((input-fp "./sample.txt")
	     ;; (x-min 200000000000000)
	     ;; (x-max 400000000000000)
	     ;; (y-min 200000000000000)
	     ;; (y-max 400000000000000)
	     (trajectories (sort (read-input input-fp) 'ttg-sort))
	     ;; (reference (first trajectories))
	     ;; (traj-sorter (make-trajectory-sort reference))
	     ;; (sorted-trajectories (append (list reference )
	     ;; 				  (sort (rest trajectories) traj-sorter)))
	     )

	

	
	
	;; (format t "Invalid dx: ~s~%" (loop for traj in trajectories
	;; 				   collect (getf traj :dx)))

	;; (format t "Invalid dy: ~s~%" (loop for traj in trajectories
	;; 				   collect (getf traj :dy)))
	
	;; (format t "Invalid dz: ~s~%" (loop for traj in trajectories
	;; 				   collect (getf traj :dz)))
	
	))

;; 27732 is the right answer for part 1
;; run time ~ 0.12 seconds
