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

(defun dump-table (table)
  (maphash (lambda (k v) (format t "~a -> ~a~%" k v))
	   table))

;;; Gets a list of all nodes reachable from the specified node
(defun parse-input (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil) (read-line in nil))
	 (values (make-array 2000 :fill-pointer 0)))
	((not line) values)
      (multiple-value-bind (left-str right-list) (split-at " <-> " line)
	(vector-push-extend
	 (list :left (parse-integer left-str)
	       :right (mapcar #'parse-integer (split-sequence:split-sequence #\, right-list)))
	 values)))))


(defclass cell () ((program-id :accessor program-id
			       :initarg :program-id
			       :initform (error "program-id must be specified")
			       :type 'integer)
		   (parent :accessor parent
			   :initform nil
			   :type 'cell)
		   (node-rank :accessor node-rank
			      :initform 0
			      :type 'integer)
		   (children :accessor children
			     :initform nil
			     :type 'list)))

(defmethod print-object ((cell cell) stream)
  (format stream "[~a (-^ ~a; #~a)]"
	  (program-id cell)
	  (if (parent cell)
	      (program-id (parent cell))
	      nil)
	  (node-rank cell)))

(defun make-cell (id) (make-instance 'cell :program-id id))

(defun get-cell (idx items)
  (when (not (elt items idx))
    (setf (elt items idx) (make-cell idx)))
  (elt items idx))

(defun find-root (item)
  (if (parent item)
      (find-root (parent item))
      item))

(defun set-parent (c p)
  (setf (parent c) p)
  (setf (children p)
	(append (list c) (children p))))

(defun add-edge (left right)
  (let ((lroot (find-root left))
	(rroot (find-root right)))
    (unless (= (program-id lroot) (program-id rroot))
      (cond ((< (node-rank lroot) (node-rank rroot)) (set-parent lroot rroot))
	    ((> (node-rank lroot) (node-rank rroot)) (set-parent rroot lroot))
	    (t (progn
		 (incf (node-rank lroot))
		 (set-parent rroot lroot)))))))

(defun generate-spanning-trees (edges)
  (let ((items (make-array (length edges) :initial-element nil)))
    (dotimes (i (length edges))
      (let ((edge (elt edges i)))
	(let ((left (get-cell (getf edge :left) items)))
	(dolist (right (getf edge :right))
	  (add-edge left (get-cell right items))))))
    items))
