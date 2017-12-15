(defun gen-a (prev) (mod (* prev 16807) 2147483647))
(defun gen-b (prev) (mod (* prev 48271) 2147483647))

(defun step-generators (prev-a prev-b)
  (let ((next-a (gen-a prev-a))
	(next-b (gen-b prev-b)))
    (let ((low-a (logand next-a #xFFFF))
	  (low-b (logand next-b #xFFFF)))
      (values next-a
	      next-b
	      (if (= low-a low-b)
		  1
		  0)))))
	
(defun run-day15a (input-a input-b)
  (let ((cur-a input-a)
	(cur-b input-b)
	(count 0))
    (dotimes (i 40000000)
      (multiple-value-bind (next-a next-b addend) (step-generators cur-a cur-b)
	(setf cur-a next-a
	      cur-b next-b
	      count (+ count addend))))
    count))
