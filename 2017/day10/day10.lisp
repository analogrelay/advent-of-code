(ql:quickload "split-sequence")

;;; Parsing again!
(defun parse-input (filename)
  (with-open-file (in filename)
    (mapcar 'parse-integer (split-sequence:split-sequence #\, (read-line in nil)))))

;;; Utility function to generate a range list
(defun range (max) (loop for n from 0 below max collect n))

;;; Split a list into two at the offset
(defun split-list (seq offset)
  (values (subseq seq 0 offset)
	  (subseq seq offset)))

;;; Calculate the necessary break-points to reverse the provided range of a list
;;; We want two points (along with the start point
;;;  'leading-end' = The end of the "leading" part (starting from 0) of the reversed segment
;;;  'trailing-end'= The end of the "trailing" part (starting from start) of the reversed segment
(defun get-break-points (start len list-len)
  (let ((end (+ start len)))
    (if (>= end list-len)
	(values (- end list-len)
		list-len)
	(values 0 end))))

;;; Reverses the requested segment of a circular list
(defun reverse-segment (seq start len)
  (let ((seq-len (length seq)))
    (multiple-value-bind (leading-end trailing-end) (get-break-points start len seq-len)
      (let ((leading-reverse (subseq seq 0 leading-end))
	    (trailing-reverse (subseq seq start trailing-end))
	    (leading-normal (subseq seq leading-end start))
	    (trailing-normal (subseq seq trailing-end seq-len)))
	(let ((reversed (nreverse (concatenate 'list trailing-reverse leading-reverse)))
	      (split-point (length trailing-reverse)))
	  (multiple-value-bind (trailing-reverse leading-reverse) (split-list reversed split-point)
	    (concatenate 'list
			 leading-reverse
			 leading-normal
			 trailing-reverse
			 trailing-normal)))))))

;;; Calculate the hash
(defun knot-hash-round (key start skip-size seq)
  (dolist (item key)
    (setf seq (reverse-segment seq start item))
    (setf start (mod (+ start item skip-size) (length seq)))
    (incf skip-size))
  (values seq start skip-size))

(defun create-key (str)
  (concatenate 'vector
	       (map 'vector #'char-int str)
	       (list 17 31 73 47 23)))

(defun sparse-knot-hash (key seq)
  (let ((start 0)
	(skip-size 0))
    (dotimes (i 64)
      (multiple-value-bind (new-seq new-start new-skip-size) (knot-hash-round key start skip-size seq)
	(setf seq new-seq
	      start new-start
	      skip-size new-skip-size))))
  seq)

(defun compact-block (seq block-index)
  (let* ((start (* block-index 16))
	 (end (+ start 16)))
    (reduce #'logxor seq :start start :end end)))

(defun compact-hash (hash)
  (let ((compacted (make-array 16 :fill-pointer 0)))
    (dotimes (i 16)
      (vector-push (compact-block hash i) compacted))
    compacted))

(defun run-day10a (filename)
  (let ((seq (knot-hash-round (parse-input filename) 0 0 (range 256))))
    (* (first seq) (second seq))))
