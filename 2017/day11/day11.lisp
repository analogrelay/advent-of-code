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
    (:nw (list -1 0 1))
    (t (error "Unknown direction: ~a" dir))))

(defun get-distance (l r)
  (destructuring-bind (lx ly lz) l
    (destructuring-bind (rx ry rz) r
      (/ (+ (abs (- rx lx))
	    (abs (- ry ly))
	    (abs (- rz lz)))
	 2))))

(defun hexstep (position direction)
  (destructuring-bind (x y z) position
    (destructuring-bind (dx dy dz) (get-offset direction)
      (list (+ x dx) (+ y dy) (+ z dz)))))

(defun hexwalk (start directions)
  (let ((pos start)
	(max-distance 0))
    (dolist (dir directions)
      (setf pos (hexstep pos dir))
      (let ((new-distance (get-distance start pos)))
	(when (> new-distance max-distance)
	  (setf max-distance new-distance))))
    (values pos max-distance)))

(defun run-day11 (filename)
  (multiple-value-bind (end-pos max-distance) (hexwalk (list 0 0 0) (parse-input filename))
    (values (get-distance (list 0 0 0) end-pos)
	    max-distance)))
