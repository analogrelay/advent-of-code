(ql:quickload "split-sequence")

;;; Parse the input
(defun parse-input (filename)
  (with-open-file (in filename)
    (let* ((line (read-line in nil))
	   (strs (remove "" (split-sequence:split-sequence #\Space line) :test #'string=))
	   (ints (mapcar #'parse-integer strs)))
      (make-array (list-length ints) :initial-contents ints))))

;;; Get the index of the largest bank
(defun find-largest-bank (banks)
  (let ((max-val (reduce #'max banks)))
    (position max-val banks)))

;;; Distribute the provided number of blocks among the banks, starting at start
(defun distribute (start amount banks)
  (dotimes (i amount)
    (incf (elt banks (mod (+ start i) (length banks)))))
  banks)

;;; Rebalance the banks by finding the largest one and redistributing it's contents
(defun rebalance (banks)
  (let* ((target-bank (find-largest-bank banks))
	 (amount (elt banks target-bank))
	 (next-bank (mod (1+ target-bank) (length banks))))
    (setf (elt banks target-bank) 0)
    (distribute next-bank amount banks)))

;;; Bank equality function. Simple item-wise equality
(defun bank= (l r)
  (and
   (= (length l) (length r))
    (every #'= l r)))

;;; Calculate the number of rebalances needed to see a pattern that has been seen before
(defun get-rebalance-count (bank)
  (do ((seen-patterns nil (cons current-bank seen-patterns))
       (current-bank bank (rebalance current-bank))
       (count 0 (1+ count)))
      ((find current-bank seen-patterns :test #'bank=) count)))
