(load "../day10/day10.lisp")

(defun set-bits (val offset array)
  (dotimes (bit 8)
    (setf (elt array (+ (* offset 8) (- 7 bit))) (logand (ash val (- bit)) 1))))

(defun to-bit-array (seq)
  (let ((arr (make-array (* (length seq) 8) :element-type 'bit :initial-element 0)))
    (dotimes (i (length seq))
      (set-bits (elt seq i) i arr))
    arr))

(defun render-row (bits)
  (dotimes (i (length bits))
    (case (elt bits i)
      (1 (format t "#"))
      (0 (format t ".")))))

(defun get-row-key (key row)
  (concatenate 'string key (format nil "-~a" row)))

(defun generate-memory (key rows)
  (let ((arr (make-array (* rows 16 8) :element-type 'bit :initial-element 0)))
    (dotimes (row rows)
      (let ((val (knot-hash (get-row-key key row))))
	(dotimes (i 16)
	  (set-bits (elt val i) (+ (* row 16) i) arr))))
    arr))
	
(defun run-day14a (input)
  (count 1 (generate-memory input 128)))
