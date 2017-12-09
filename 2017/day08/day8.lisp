(ql:quickload "split-sequence")

(defun make-cpu () (make-hash-table :test #'equal))

(defun dump-cpu (cpu)
  (maphash (lambda (k v) (format t "~a => ~a~%" k v)) cpu))

(defun register (name cpu)
  ;(format t "read ~a~%" name)
  (or (gethash name cpu) 0))

(defun (setf register) (new-value name cpu)
  ;(format t "~a <- ~a~%" name new-value)
  (setf (gethash name cpu) new-value))

(defun run-opcode (s l r)
  (cond ((equal s "inc") (+ l r))
	((equal s "dec") (- l r))
	(t nil)))

(defun run-comparison (x l r)
  (cond ((equal x ">") (> l r))
	((equal x "<") (< l r))
	((equal x ">=") (>= l r))
	((equal x "<=") (>= l r))
	((equal x "==") (= l r))
	((equal x "!=") (/= l r))))

(defun parse-instruction (inst)
  (let ((splat (split-sequence:split-sequence #\Space inst)))
    (list (pop splat)
	  (pop splat)
	  (parse-integer (pop splat))
	  (progn
	    (pop splat) ; Disregard the word "if"
	    (pop splat)) ; Keep the register name
	  (pop splat)
	  (parse-integer (pop splat)))))

(defun log-inst (inst &optional new-val)
  (if new-val
      (format t "~a ==> ~a <- ~a~%" inst (nth 0 inst) new-val)
      (format t "~a SKIP~%" inst)))

(defun compile-instruction (inst)
  (destructuring-bind (dest opcode operand comparee comparison comparand) inst
    (lambda (cpu)
      (let* ((dest-val (register dest cpu))
	     (comparee-val (register comparee cpu))
	     (new-val (run-opcode opcode dest-val operand)))
	(if (run-comparison comparison comparee-val comparand)
	    (progn
	      (log-inst inst new-val)
	      (setf (register dest cpu) new-val))
	    (log-inst inst))))))
	  
(defun parse-program (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil) (read-line in nil))
	 (insts nil (cons (parse-instruction line) insts)))
	((not line) (nreverse insts)))))

(defun compile-program (insts)
  (mapcar #'compile-instruction insts))

(defun run-prog (compiled cpu)
  (mapcar (lambda (f) (funcall f cpu)) compiled))

(defun run-day8a (filename)
  (let ((compiled-program (compile-program (parse-program filename)))
	(cpu (make-cpu)))
    (run-prog compiled-program cpu)
    (let ((regs nil))
      (maphash (lambda (k v) (push (list k v) regs)) cpu)
      (reduce #'max regs :key #'cadr))))
