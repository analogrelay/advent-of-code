(ql:quickload "split-sequence")

(defun parse-input (filename)
  (with-open-file (in filename)
    (loop for l = (read-line in nil)
       while l
       collect
	 (split-sequence:split-sequence #\Space l))))

(defun count-string (item list)
  (count-if (lambda (x) (equal x item)) list))

(defun check-passphrase (phrase)
  (member-if (lambda (x) (> (count-string x phrase) 1)) phrase))

(defun run-day4a (filename)
  (count-if #'check-passphrase (parse-input filename)))
