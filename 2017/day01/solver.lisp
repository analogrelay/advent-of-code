(defun parse-input (filename)
  (with-open-file (in filename)
    (loop for c = (read-char in nil)
	 while (and (char>= c #\0) (char<= c #\9))
	 collect (parse-integer (string c)))))

(defun solve-captcha (input)
  (getf (reduce (lambda (state next)
	    (let ((sum (getf state :sum))
		  (last (getf state :last)))
	      (list
	       :sum (if (eql next last) (+ sum next) sum)
	       :last next)))
	  input
	  :initial-value (list :sum 0 :last (first (last input))))
	:sum))

(defun run-day1a (filename)
  (solve-captcha (parse-input filename)))
