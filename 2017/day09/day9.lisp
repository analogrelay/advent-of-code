;;; Get the next token from the stream
(defun next-token (in)
  (let ((c (read-char in nil)))
    (case c
	(#\{ (list :type :start-group))
	(#\} (list :type :end-group))
	(#\, (list :type :comma))
	(#\< (read-garbage in))
	((#\Space #\Tab #\Newline) (list :type :ws))
	((nil) nil)
	(otherwise (error "unexpected character: '~a'" c)))))

;;; Tokenize a stream until the end
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
      ((and (not in-escape)
	    (eql c #\>))
       (progn
	 (read-char in nil) ; Skip the ">"
	 (list :type :garbage :value garbage-string)))
    ;; If we're in an escape
    (if in-escape
	;; Just skip the character and clear the in-escape flag
	(setf in-escape nil)
	;; Otherwise, if we see an escape
	(if (eql c #\!)
	    ;; Set the flag and ignore the character
	    (setf in-escape t)
	    ;; Otherwise, add the character
	    (vector-push-extend c garbage-string)))

    ;; Advance to the next character
    (read-char in nil)))

(defun is-token (token test-type)
  (eql (getf token :type) test-type))

(defun expect-token (token-stream expected-type)
  (let ((tok (first token-stream)))
    (if (is-token tok expected-type)
	(rest token-stream)
	(error "unexpected token: ~a" tok))))

(defun get-group-score (token-stream base-score)
  ;; Read the initial start-group token
  (setf token-stream (expect-token token-stream :start-group))

  ;; Iterate over tokens
  (do ((tok (first token-stream) (first token-stream))
       (base-score (1+ base-score))
       (total-score (1+ base-score)))

      ;; End the stream at an end-group token. Return the total score, and the remaining tokens
      ((is-token tok :end-group) (values total-score (expect-token token-stream :end-group)))
    ;; If this is a start-group token
    (if (is-token tok :start-group)
	;; Recursively parse that group and move up
	(multiple-value-bind (group-score remaining-tokens) (get-group-score token-stream base-score)
	  ;; Update score and token stream from the values
	  (incf total-score group-score)
	  (setf token-stream remaining-tokens))
	;; Otherwise, skip the token
	(pop token-stream))))

(defun run-day9a (filename)
  (get-group-score (tokenize-file filename) 0))
