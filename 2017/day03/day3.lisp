;;; Determines the side length of the memory with the provided size
;;;
;;; The dimensions are given the nearest odd number above ceil(sqrt(size)),
;;; since the memory is always a perfect square.
(defun get-memory-dimension (size)
  (let ((dim (ceiling (sqrt size))))
    (if (eql (mod dim 2) 0)
	(1+ dim)
	dim)))

(defun get-coords (addr)
  (let ((dim (get-memory-dimension addr)))
    (let ((min-addr (* (- dim 2) (- dim 2)))
	  (max-addr (* dim dim)))
      (let ((right-break (+ min-addr (- dim 1)))
	    (top-break (+ min-addr (- (* dim 2) 2)))
	    (left-break (+ min-addr (- (* dim 3) 3)))
	    (bottom-break (+ min-addr (- (* dim 4) 4))))
	(if (eql addr max-addr)
	    (list (1- dim) (1- dim))
	    (if (<= addr right-break)
		(list (1- dim) (- right-break addr))
		(if (<= addr top-break)
		    (list (- top-break addr) 0)
		    (if (<= addr left-break)
			(list 0 (- left-break addr))
			(list (- (1- dim) (- bottom-break addr)) (1- dim))))))))))
      
(defun get-taxi-distance (target dest)
  (destructuring-bind (tx ty) target
    (destructuring-bind (dx dy) dest
      (+ (abs (- dx tx)) (abs (- dy ty))))))

(defun get-memory-center (dim)
  (let ((radius (floor (/ dim 2))))
    (list radius radius)))

(defun run-day3a (input)
  (get-taxi-distance (get-coords input) (get-memory-center (get-memory-dimension input))))
