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

;;; Accessors

(defun name (node) (getf node :name))
(defun weight (node) (getf node :weight))
(defun children (node) (getf node :children))

;;; More helpers

(defun dump-tree (tree)
  (maphash (lambda (k v) (format t "~a => ~a~%" k v)) tree))

(defun find-base-node (tree)
  (let ((base-node nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (when (not (getf v :parent)) (setf base-node v)))
	     tree)
    base-node))

(defun find-children (node tree)
  (let ((child-names (children node)))
    (if child-names
      (mapcar (lambda (x)
		(gethash x tree)) child-names))))

(defun get-total-weight (node tree)
  ;; Using initial-value is tricky here because of the use of :key
  ;; So we'll just add it after the reduction
  (+ (weight node)
     (reduce #'+ (find-children node tree) :key (lambda (x) (get-total-weight x tree)))))

;;; Find the outlier of a list of nodes
(defun find-outlier (items &key key)
  (let ((max-val (reduce #'max items :key key))
	(min-val (reduce #'min items :key key)))
    (if (= max-val min-val)
	nil
	(if (= (count max-val items :key key) 1)
	    max-val
	    min-val))))

(defun get-unbalanced-node (base tree)
  ;; Get child-nodes and their total weights
  (flet ((get-total-weight (x) (get-total-weight x tree))
	 (find-children (x) (find-children x tree)))
    (let* ((child-nodes (find-children base))
	   (child-weights (mapcar #'get-total-weight child-nodes))
	   (outlier-weight (find-outlier child-weights))
	   (outlier-pos (position outlier-weight child-weights))
	   (outlier-node (nth outlier-pos child-nodes)))
      outlier-node)))
  
  
(defun run-day7a (filename)
  (find-base-node (parse-input filename)))
