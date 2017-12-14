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

;;; firewall class
(defclass firewall () ((index :reader index
			      :initarg :index
			      :initform (error "The :index parameter must be provided")
			      :type 'integer)
		       (depth :reader depth
			      :initarg :depth
			      :initform (error "The :depth parameter must be provided")
			      :type 'integer)
		       (scan-position :accessor scan-position
				      :initform nil 
				      :type 'integer)))

(defmethod print-object ((layer firewall) stream)
  (format stream "~a: " (index layer))
  (dotimes (i (depth layer))
    (format stream
	    (let ((scan-position (scan-position layer)))
	      (if (and scan-position (= i scan-position))
		  "[S] "
		  "[ ] ")))))

;;; game-board class
(defclass game-board () ((layers :reader layers
				 :initarg :layers
				 :initform (error "The :layers parameter must be provided")
				 :type 'list)
			 (player-position :accessor player-position
					  :initform nil)))

(defmethod print-object ((board game-board) stream)
  (let ((player-pos (player-position board))
	(layers (layers board)))
    (do ((index 0 (1+ index))
	 (next-layer (first layers) (first layers)))
	((not layers) nil)
      (let* ((layer (when (= (index next-layer) index) (pop layers)))
	     (scan-pos (when layer (scan-position layer)))
	     (depth (if layer (depth layer) 0)))
	(format stream "~a: " index)
	(dotimes (i depth)
	  (if (and player-pos (= player-pos index))
	      (if (and scan-pos (= scan-pos i))
		  (format stream "(S) ")
		  (format stream "( ) "))
	      (if (and scan-pos (= scan-pos i))
		  (format stream "[S] ")
		  (format stream "[ ] "))))
	(format stream "~%"))))))


(defun parse-line (line)
  (multiple-value-bind (layer depth) (split-at ": " line)
    (make-instance 'firewall
		   :index (parse-integer layer)
		   :depth (parse-integer depth))))

(defun parse-input (filename)
  (with-open-file (in filename)
    (make-instance 'game-board :layers
		   (do ((line (read-line in nil) (read-line in nil))
			(input nil))
		       ((not line) (nreverse input))
		     (push (parse-line line) input)))))

(defgeneric step-game (component))

(defmethod step-game ((component firewall))
  (setf (scan-position component) (let ((scan-pos (scan-position component)))
				    (if scan-pos
					(mod (1+ (scan-position component)) (depth component))
					0)))
  component)
