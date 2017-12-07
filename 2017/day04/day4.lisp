(ql:quickload "split-sequence")

(defun parse-input (filename)
  (with-open-file (in filename)
    (loop for l = (read-line in nil)
       while l
       collect
	 (split-sequence:split-sequence #\Space l))))

(defun check-passphrase (test phrase)
  (every (lambda (x) (funcall test x phrase)) phrase))

(defun has-duplicate (item phrase)
  (> (count item phrase :test #'string=) 1))

(defun has-anagram (item phrase)
  (> (count item phrase :test #'anagramp) 1))

(defun anagramp (l r)
  (equal (sort (copy-seq l) #'string<) (sort (copy-seq r) #'string<)))

(defun count-passphrases (test list)
  (count-if (lambda(x) (check-passphrase test x)) list))

(defun run-day4a (filename)
  (count-passphrases #'has-duplicate (parse-input filename)))

(defun run-day4b (filename)
  (count-passphrases #'has-anagram (parse-input filename)))
