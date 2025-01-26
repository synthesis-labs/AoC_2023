(ql:quickload "str")

(let ((input-fl "./input.txt")
      (num-cards 0)
      (copies (make-hash-table)))

  (with-open-file (stream input-fl)

    (loop for idx from 1 for line = (read-line stream 'nil) while line do

      (setf (gethash idx copies 0) (1+ (gethash idx copies 0)))
      (setf num-cards (+ num-cards (gethash idx copies)))
      
      (let* ((details (str:trim (second (str:split #\: line))))
	     (winning-half (str:trim (first (str:split #\| details))))
	     (winners (remove "" (str:split #\Space winning-half) :test 'equal))
	     (elfs-half (str:trim (second (str:split #\| details))))
	     (elfs (remove "" (str:split #\Space elfs-half) :test 'equal))
	     (num-matches (length (intersection winners elfs :test 'equal))))
	
	(loop for win from (1+ idx) to (+ idx num-matches) do
	  (setf (gethash win copies) (+ (gethash idx copies) (gethash win copies 0)))))))

  (print num-cards))
