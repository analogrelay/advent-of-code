;;; Get the next token from the stream
(defun next-token (in)
  (case (read-char in nil)
    (#\{ (list :type :start-group))
    (#\} (list :type :end-group))))

(defun tokenize (in)
  (do ((tok (next-token in) (next-token in))
       (tokens nil))
      ((not tok) (nreverse tokens))
    (push tok tokens)))
   
(defun tokenize-string (str)
  (with-input-from-string (in str)
    (tokenize in)))
   
(defun tokenize-file (filename)
  (with-open-file (in filename)
    (tokenize in)))

(defun read-garbage (in)
  (do ((c (peek-char nil in) (peek-char nil in))
       (in-escape nil)
       (garbage-string (make-array 8 :fill-pointer 0 :adjustable t :element-type 'character)))
      ((and (not in-escape) (eql c #\>)) (list :type :garbage :value garbage-string))
    ;; If we're in an escape
    (if in-escape
	;; Just add the character and clear the in-escape flag
	(progn
	  (vector-push-extend c garbage-string)
	  (setf in-escape nil))
	;; Otherwise, if we see an escape
	(if (eql c #\!)
	    ;; Set the flag and ignore the character
	    (setf in-escape t)
	    ;; Otherwise, add the character
	    (vector-push-extend c garbage-string)))

    ;; Advance to the next character
    (read-char in nil)))
