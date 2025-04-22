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
      (list :x x :y y :z z :dx dx :dy dy :dz dz))))


(defun re-parameterize (line)
  (destructuring-bind (&key x y dx dy &allow-other-keys) line
    (list :x x
	  :y y
	  :dx dx
	  :dy dy
	  :m (/ dy dx)
	  :c (- y (* (/ dy dx) x)))))


(defun read-input (input-fp)
  (with-open-file (stream input-fp)
    (loop for idx from 0
	  for line = (read-line stream 'nil)  while line
	  collect (re-parameterize (parse-line line)))))


(defun collision-point (hailstone1 hailstone2)
  (let ((m1 (getf hailstone1 :m))
	(c1 (getf hailstone1 :c))
	(m2 (getf hailstone2 :m))
	(c2 (getf hailstone2 :c)))
    (if (/= m1 m2)
	(let* ((x (/ (- c2 c1) (- m1 m2)))
	       (y (+ (* m1 x) c1))
	       (t1 (/ (- y (getf hailstone1 :y)) (getf hailstone1 :dy)))
	       (t2 (/ (- y (getf hailstone2 :y)) (getf hailstone2 :dy))))

	(list :x (/ (- c2 c1) (- m1 m2))
	      :y (+ (* m1 (/ (- c2 c1) (- m1 m2))) c1)
	      :t1 t1
	      :t2 t2)))))


(defun count-collisions (trajectories x-min x-max y-min y-max)
  (let ((collisions 0))

    (loop for a from 0 below (1- (length trajectories)) do
      (loop for b from (1+ a) below (length trajectories) do
	(let ((collision (collision-point (nth a trajectories)
					  (nth b trajectories))))
	  (if collision
	      (destructuring-bind (&key x y t1 t2) collision
		(if (and (<= x-min x) (<= x x-max)
			 (<= y-min y) (<= y y-max)
			 (<= 0 t1)
			 (<= 0 t2))
		    (incf collisions)))))))
    collisions))


(time (let* ((input-fp "./input.txt")
	     (x-min 200000000000000)
	     (x-max 400000000000000)
	     (y-min 200000000000000)
	     (y-max 400000000000000)
	     (trajectories (read-input input-fp)))

	(format t "Total collisions: ~s~%"
		(count-collisions trajectories x-min x-max y-min y-max))))

;; 27732 is the right answer for part 1
;; run time ~ 0.12 seconds
