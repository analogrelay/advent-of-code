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

(defun row-checksum (row)
  (- (reduce #'max row) (reduce #'min row)))

(defun checksum (checksum-fn table)
  (reduce #'+ (mapcar checksum-fn table)))

(defun run-day2a (filename)
  (checksum #'row-checksum (parse-input filename)))
