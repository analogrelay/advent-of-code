;;; Parse the input as a sequence of integers
(defun parse-input (filename)
  (with-open-file (in filename)
    (loop for c = (read-char in nil)
	 while (and (char>= c #\0) (char<= c #\9))
	 collect (parse-integer (string c)))))

;;; Utility function to generate a range list
(defun range (max)
  (loop for n from 0 below max
       collect n))

;;; Solve the "captcha" provided in the specified list
;;; This needs to support both part a and part b methods
(defun solve-captcha (input offset)
  (reduce (lambda (sum idx)
	    (let ((cur (nth idx input))
		  (target (nth (mod (+ idx offset) (list-length input)) input)))
	      (if (eql cur target)
		  (+ sum cur)
		  sum)))
	  (range (list-length input))
	  :initial-value 0))

;;; Run the day 1a solution on the specified input file
(defun run-day1a (filename)
  (solve-captcha (parse-input filename) 1))

;;; Run the day 1b solution on the specified input file
(defun run-day1b (filename)
  (let ((input (parse-input filename)))
    (solve-captcha input (/ (list-length input) 2))))
