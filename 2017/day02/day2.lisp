;;; Load dependencies
(ql:quickload "split-sequence")

;;; Parse the input as a sequence of tab/newline separated integers
(defun parse-input (filename)
  (with-open-file (in filename)
    (loop for l = (read-line in nil)
       while l
       collect
	 (mapcar (lambda (x) (parse-integer x))
		 (split-sequence:split-sequence #\Tab l)))))

(defun min-max-checksum (row)
  (- (reduce #'max row) (reduce #'min row)))

(defun div-checksum (row)
  (loop named outer for l in row do
    (loop for r in row
	when (and (/= l r) (eql (mod l r) 0))
	do (return-from outer (/ l r)))))

(defun checksum (checksum-fn table)
  (reduce #'+ (mapcar checksum-fn table)))

(defun run-day2a (filename)
  (checksum #'min-max-checksum (parse-input filename)))

(defun run-day2b (filename)
  (checksum #'div-checksum (parse-input filename)))
