(ql:quickload "split-sequence")

;;; Helper functions
(defun split-seq (position list &optional (separator-length 0))
  (values (subseq list 0 position)
	  (subseq list (+ position separator-length))))

(defun split-at (candidate seq)
  (let ((pos (search candidate seq)))
    (if pos
	(split-seq (search candidate seq) seq (length candidate))
	(values seq nil))))

;;; Parsers
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

(defun parse-input (filename)
  (let ((entries (make-hash-table :test #'equal)))
    (with-open-file (in filename)
      (do ((line (read-line in nil) (read-line in nil)))
	  ((not line) entries)
	(let ((entry (parse-entry line)))
	  (setf (gethash (getf entry :name) entries) entry))))
    ;; Link children to parents
    (maphash (lambda (k v)
	       (dolist (child (getf v :children))
		 (setf (getf (gethash child entries) :parent) k)))
	     entries)
    entries))

(defun dump-tree (tree)
  (maphash (lambda (k v) (format t "~a => ~a~%" k v)) tree))

(defun find-base-node (tree)
  (let ((base-node nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (when (not (getf v :parent)) (setf base-node v)))
	     tree)
    base-node))

(defun run-day7a (filename)
  (find-base-node (parse-input filename)))
