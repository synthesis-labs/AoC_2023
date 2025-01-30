(defun read-input-data (input-stream &optional accum)
  (let ((line (read-line input-stream 'nil)))
    (if (null line)
	accum
	(let* ((split-pos (position #\Space line))
	       (hand (subseq line 0 split-pos))
	       (bet (parse-integer (subseq line split-pos) :junk-allowed t)))
	  (read-input-data input-stream (push (cons hand bet) accum))))))

(defun card-to-val (input-char &optional (multiplier 1))
  (let ((card-vals "23456789TJQKA"))
    (* multiplier (position input-char card-vals))))

(defun score-hand (hand)
  (let ((char-counts (make-hash-table :test 'eql))
	(chars (loop for char across hand collect char))
	(hand-vals '()))

    (loop for char in chars do
      (setf (gethash char char-counts) (1+ (gethash char char-counts 0))))

    (setf hand-vals
	  (sort (loop for v being the hash-value of char-counts collect v) #'>))

    (let ((top (car hand-vals))
	  (sec (cadr hand-vals)))
      
      (cond ((= top 5) 100) ;; 5-of-a-kind
	    ((= top 4) 90) ;; 4-of-a-kind
	    ((and (= top 3) (= sec 2)) 80) ;; full-house
	    ((= top 3) 70) ;; 3-of-a-kind
	    ((and (= top 2) (= sec 2)) 60) ;; two-pair
	    ((= top 2) 50) ;; pair
	    ;; (t (apply 'max (map 'list 'card-to-val hand))) ;; Very confusing
	    (t 0)))))

(defun sort-sequential (a b)
  (if (= 0 (length a)) (return-from sort-sequential 'nil))
  (let ((alpha (card-to-val (char a 0)))
	(beta (card-to-val (char b 0))))

    (cond ((< alpha beta) 'T)
	  ((< beta alpha) 'nil)
	  (t (sort-sequential (subseq a 1) (subseq b 1))))))


(defun sort-hands (a b)
  (let ((alpha (score-hand (car a)))
	(beta (score-hand (car b))))
    (if (= alpha beta)
	(sort-sequential (car a) (car b))
	(< alpha beta))))

(let ((input-fp "./input.txt")
      (input-data '())
      (output-data '())
      (winnings 0))

  (with-open-file (stream input-fp)
    (setf input-data (read-input-data stream)))
  (setf output-data (stable-sort input-data 'sort-hands))
  (setf winnings (apply '+ (loop for rank from 1
				 for el in output-data
				 collect (* rank (cdr el)))))
  (format t "Winnings: ~s~%" winnings))

;; 245813737 is too low for part 1
;; 245813737
;; 246163188 is the right answer for part 1 ... the "High card" actually counting as nothing threw me
