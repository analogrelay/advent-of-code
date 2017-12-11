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
	(values start end))))

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
(defun knot-hash (lengths &optional (key 256))
  (let ((seq (range key))
	(start 0)
	(skip-size 0))
    (dolist (len lengths)
      (setf seq (reverse-segment seq start len))
      (setf start (mod (+ start len skip-size) key))
      (incf skip-size))
    (* (first seq) (second seq))))

(defun run-day10a (filename)
  (knot-hash (parse-input filename)))
