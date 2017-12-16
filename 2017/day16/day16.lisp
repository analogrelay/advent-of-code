(ql:quickload "split-sequence")

;;; Split a list into two at the offset
(defun split-list (seq offset)
  (values (subseq seq 0 offset)
	  (subseq seq offset)))

;;; Spin a list X places
(defun spin-list (seq offset)
  (multiple-value-bind (before after) (split-list seq (- (length seq) offset))
    (append after before)))

;;; Exchange items at places X and Y
(defun exchange-list (seq x y)
  (let ((local-seq (copy-seq seq)))
    (let ((temp (elt seq x)))
      (setf (elt local-seq x) (elt seq y))
      (setf (elt local-seq y) temp))
    local-seq))

;;; Partner items X and Y
(defun partner-list (seq x y)
  (let ((pos-x (position x seq))
	(pos-y (position y seq)))
    (exchange-list seq pos-x pos-y)))

(defun parse-spin (move)
  (list :spin (parse-integer move)))

(defun parse-exchange (move)
  (let ((targets (split-sequence:split-sequence #\/ move)))
    (list :exchange
	  (parse-integer (elt targets 0))
	  (parse-integer (elt targets 1)))))

(defun parse-partner (move)
  (let ((targets (split-sequence:split-sequence #\/ move)))
    (list :partner
	  (elt (elt targets 0) 0)
	  (elt (elt targets 1) 0))))

(defun parse-move (move)
  (case (elt move 0)
    (#\s (parse-spin (subseq move 1)))
    (#\x (parse-exchange (subseq move 1)))
    (#\p (parse-partner (subseq move 1)))))

(defun parse-moves (str)
  (mapcar #'parse-move
	  (split-sequence:split-sequence #\, str)))

(defun parse-input (filename)
  (with-open-file (in filename)
    (parse-moves (read-line in nil))))

(defun run-spin (move dance-floor)
  (destructuring-bind (m offset) move
    (declare (ignore m))
    (spin-list dance-floor offset)))

(defun run-partner (move dance-floor)
  (destructuring-bind (m x y) move
    (declare (ignore m))
    (partner-list dance-floor x y)))

(defun run-exchange (move dance-floor)
  (destructuring-bind (m x y) move
    (declare (ignore m))
    (exchange-list dance-floor x y)))

(defun run-move (move dance-floor)
  (case (first move)
    (:spin (run-spin move dance-floor))
    (:partner (run-partner move dance-floor))
    (:exchange (run-exchange move dance-floor))))

(defun run-dance (moves dance-floor)
  (let ((current-floor dance-floor))
    (dolist (move moves)
      (setf current-floor (run-move move current-floor)))
    current-floor))

(defun make-floor (str)
  (let ((lst nil))
    (dotimes (i (length str))
      (push (elt str i) lst))
    (nreverse lst)))

(defun render-floor (lst) (concatenate 'string lst))

(defun run-day16a (filename)
  (render-floor (run-dance (parse-input filename) (make-floor "abcdefghijklmnop"))))

(defun get-all-results (moves floor)
  (let* ((dance-floor floor)
	 (results (list floor)))
    ;; Determine how many cycles make up a loop
    (dotimes (i 100)
      (let ((new-floor (run-dance moves dance-floor)))
	(setf dance-floor new-floor)
	(when (equal (render-floor dance-floor) (render-floor floor))
	    (return (nreverse results)))
	(push new-floor results)))))

(defun run-day16b (filename)
  (render-floor
   (let ((all-dances (get-all-results (parse-input filename) (make-floor "abcdefghijklmnop"))))
     (format t "found ~a dances make a cycle, so we need dance ~a of the cycle"
	     (length all-dances)
	     (mod 1000000000 (length all-dances)))  
     (nth (mod 1000000000 (length all-dances)) all-dances))))
