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
				      :initform 0 
				      :type 'integer)))

(defun severity (firewall) (* (depth firewall) (index firewall)))

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
			 (last-layer :reader last-layer
				     :initarg :last-layer
				     :initform (error "The :last-layer parameter must be provided")
				     :type 'integer)
			 (total-severity :accessor total-severity
					 :initform 0
					 :type 'integer)
			 (player-position :accessor player-position
					  :initform nil)))

(defmethod print-object ((board game-board) stream)
  (format stream "severity so far: ~a~%" (total-severity board))
  (let ((player-pos (or (player-position board) -1))
	(layers (layers board)))
    (do ((index 0 (1+ index))
	 (next-layer (first layers) (first layers)))
	((not layers) nil)
      (let* ((layer (when (= (index next-layer) index) (pop layers)))
	     (scan-pos (when layer (scan-position layer)))
	     (depth (if layer (depth layer) 0)))
	(format stream "~a: " index)
	(if (and (= depth 0) (= player-pos index))
	    (format stream "( ) ")
	    (dotimes (i depth)
	      (if (and (= i 0) (= player-pos index))
		  (if (and scan-pos (= scan-pos i))
		      (format stream "(S) ")
		      (format stream "( ) "))
		  (if (and scan-pos (= scan-pos i))
		      (format stream "[S] ")
		      (format stream "[ ] ")))))
	(format stream "~%")))))


(defun parse-line (line)
  (multiple-value-bind (layer depth) (split-at ": " line)
    (make-instance 'firewall
		   :index (parse-integer layer)
		   :depth (parse-integer depth))))

(defun parse-input (filename)
  (with-open-file (in filename)
    (let ((layers (do ((line (read-line in nil) (read-line in nil))
		       (input nil))
		      ((not line) input)
		    (push (parse-line line) input))))
      (let ((last-layer (depth (first layers))))
	(make-instance 'game-board :layers (nreverse layers) :last-layer last-layer)))))

(defgeneric step-component (component))

(defmethod step-component ((component firewall))
  (setf (scan-position component)
	(let ((scan-pos (scan-position component)))
	  (if scan-pos
	      (mod (1+ (scan-position component)) (depth component))
	      0)))
  component)

(defun scanner-active (firewall) (= 0 (or (scan-position firewall) -1)))

(defmethod step-component ((component game-board))
  ;; Move the player
  (let ((pos (player-position component)))
    (setf (player-position component) (1+ (or pos -1))))
  ;; Scan all the firewalls
  (let ((pos (player-position component)))
    (dolist (layer (layers component))
      ;; If the player is in this layer BEFORE we step it forward
      ;; AND the scanner is active
      (when (and pos (= pos (index layer)) (scanner-active layer))
	;; Then they are caught
	(incf (total-severity component) (severity layer)))
      (step-component layer))
    component))

(defgeneric reset-component (component))

(defmethod reset-component ((component firewall))
  (setf (scan-position component) nil))

(defmethod reset-component ((component game-board))
  (setf (player-position component) nil)
  (dolist (layer (layers component))
    (reset-component layer)))
