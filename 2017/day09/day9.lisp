;;; Get the next token from the stream
(defun next-token (in)
  (case (read-char in nil)
    (#\{ :start-group)
    (#\} :end-group)))

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
