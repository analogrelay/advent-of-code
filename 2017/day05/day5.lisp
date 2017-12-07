(defun parse-input (filename)
  (with-open-file (in filename)
    (do ((v (make-array 1024 :fill-pointer 0))
	 (l (read-line in nil) (read-line in nil)))
	((not l) v)
      (vector-push-extend (parse-integer l) v))))
      
(defun step-program (pc sequence)
  (let ((inst (elt sequence pc)))
    (incf (elt sequence pc))
    (+ pc inst)))

(defun run-sequence (program)
  (do ((pc 0 (step-program pc program))
       (counter 0 (1+ counter)))
      ((or (< pc 0) (>= pc (length program))) counter)))

(defun run-day5a (filename)
  (run-sequence (parse-input filename)))
