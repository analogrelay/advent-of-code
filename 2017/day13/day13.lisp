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
		       (direction :accessor direction
				  :initform 1
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
			 (caught :accessor caught
				 :initform nil)
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
      (let ((last-layer (index (first layers))))
	(make-instance 'game-board :layers (nreverse layers) :last-layer last-layer)))))

(defgeneric step-component (component &key))

(defmethod step-component ((component firewall) &key)
  (let ((scan-pos (scan-position component)))
    ;; If we're at the bottom, moving down
    (cond ((and (> (direction component) 0) (= scan-pos (1- (depth component))))
	   ;; Change directions
	   (setf (direction component) -1))
	  ;; If we're at the top, moving up
	  ((and (< (direction component) 0) (= scan-pos 0))
	   ;; Change directions
	   (setf (direction component) 1)))
    ;; Apply the direction
    (incf (scan-position component) (direction component))))

(defun scanner-active (firewall) (= 0 (or (scan-position firewall) -1)))

(defmethod step-component ((component game-board) &key skip-player)
  ;; Move the player
  (unless skip-player
    (let ((pos (player-position component)))
      (setf (player-position component) (1+ (or pos -1)))))
  ;; Scan all the firewalls
  (let ((pos (player-position component)))
    (dolist (layer (layers component))
      ;; If the player is in this layer BEFORE we step it forward
      ;; AND the scanner is active
      (when (and pos (= pos (index layer)) (scanner-active layer))
	;; Then they are caught
	;;(format t "caught in ~a~%" (index layer))
	(setf (caught component) t)
	(incf (total-severity component) (severity layer)))
      (step-component layer))
    component))

(defgeneric reset-component (component))

(defmethod reset-component ((component firewall))
  (setf (scan-position component) 0))

(defmethod reset-component ((component game-board))
  (setf (player-position component) nil)
  (setf (total-severity component) 0)
  (setf (caught component) nil)
  (dolist (layer (layers component))
    (reset-component layer)))

(defun run-game (board &key delay)
  (dotimes (i (or delay 0))
    (step-component board :skip-player t))
  (do ()
      ((> (or (player-position board) -1) (last-layer board)) board)
    (step-component board)))

(defun run-day13a (filename)
  (total-severity (run-game (parse-input filename))))

;;; Determine the position of a firewall at the specified time-slice
(defun position-at-time (firewall time-slice)
  ;;; Determine the position
  (let* ((depth (depth firewall))
	 (x (* (1- depth) 2))
	 (pos (mod time-slice x)))
    ;; Pos is actually not the position, we have to figure out if it's on the upswing
    ;; or the downswing
    (if (> pos (1- depth))
	(- x pos)
	pos)))

(defun would-be-caught (board start-time)
  (dolist (layer (layers board))
    ;; Get the position at this layer
    (let ((firewall-pos (position-at-time layer (+ start-time (index layer)))))
      (when (= firewall-pos 0)
	(return-from would-be-caught t)))))

(defun run-day13b (filename &key (patience 10000000))
  (let ((board (parse-input filename)))
    (dotimes (i patience) ; Patience value just to prevent infinite loop
      (when (not (would-be-caught board i))
	(return-from run-day13b i)))))

  
