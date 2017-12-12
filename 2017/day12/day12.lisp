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

;;; Adds a unidirectional link from l to r in the table
(defun add-link (l r table)
  (setf (gethash l table)
	(adjoin r (gethash l table))))

(defun dump-table (table)
  (maphash (lambda (k v) (format t "~a -> ~a~%" k v))
	   table))

;;; Gets a list of all nodes reachable from the specified node
(defun expand-reachable (node table)
  (do ((work (list node))
       (reachable nil))
      ((not work) reachable)
    (let* ((next (pop work))
	   (children (gethash next table)))
      (dolist (child children)
	(when (not (member child reachable))
	  ;; Found a node not already reachable, add to work list
	  (push child work)
	  ;; And mark reachable
	  (push child reachable))))))

(defun parse-input (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil) (read-line in nil))
	 (links (make-hash-table)))
	((not line) links)
      (multiple-value-bind (left right) (split-at " <-> " line)
	(let ((leftidx (parse-integer left)))
	  (dolist (target (split-sequence:split-sequence #\, right))
	    (let ((targetidx (parse-integer target)))
	      (add-link leftidx targetidx links)
	      (add-link targetidx leftidx links))))))))

(defun expand-node-table (table)
  (let ((new-table (make-hash-table)))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (setf (gethash k new-table) (expand-reachable k table)))
	     table)
    new-table))

(defun run-day12a (filename)
  (length (gethash 0 (expand-node-table (parse-input filename)))))
