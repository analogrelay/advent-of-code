(ql:quickload "split-sequence")

;;; Parsing
(defun parse-direction (dir)
  (cond ((equal dir "n") :n)
	((equal dir "ne") :ne)
	((equal dir "se") :se)
	((equal dir "s") :s)
	((equal dir "sw") :sw)
	((equal dir "nw") :nw)
	(t (error "Unknown direction: ~a" dir))))

(defun parse-input (filename)
  (with-open-file (in filename)
    (mapcar #'parse-direction (split-sequence:split-sequence #\, (read-line in nil)))))

;;; Get the cube-coordinate offset given a direction
(defun get-offset (dir)
  (case dir
    (:n (list -1 1 0))
    (:ne (list 0 1 -1))
    (:se (list 1 0 -1))
    (:s (list 1 -1 0))
    (:sw (list 0 -1 1))
    (:se (list -1 0 1))))
    
