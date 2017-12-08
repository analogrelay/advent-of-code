(ql:quickload "split-sequence")

(defun split-seq (position list &optional (separator-length 0))
  (values (subseq list 0 position)
	  (subseq list (+ position separator-length))))

(defun split-at (candidate seq)
  (let ((pos (search candidate seq)))
    (if pos
	(split-seq (search candidate seq) seq (length candidate))
	(values seq nil))))

(defun parse-name-and-weight (val)
  (multiple-value-bind (name weight) (split-at " " val)
    (values name (parse-integer (string-trim '(#\( #\)) weight)))))

(defun parse-child-list (val)
  (mapcar (lambda (x) (string-trim '(#\Space) x))
	  (split-sequence:split-sequence #\, val)))

(defun parse-entry (entry)
  (multiple-value-bind (name-and-weight children) (split-at " -> " entry)
    (multiple-value-bind (name weight) (parse-name-and-weight name-and-weight)
      (let ((base-list (list :name name :weight weight)))
	(if children
	    (concatenate 'list base-list (list :children (parse-child-list children)))
	    base-list)))))

;;; Parse the input
(defun parse-input (filename)
  (with-open-file (in filename)
    (let ((entries nil))
      (do ((line (read-line in nil) (read-line in nil)))
	  (line entries)
	(setf entries (cons entries (parse-entry line)))))))
