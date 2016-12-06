(cl:in-package :sicl-sequence)

(defun fill-list (list item start end)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (for-each-relevant-cons (cons index list start end nil)
    (setf (car cons) item)))

(defun fill-vector (item vector start end)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (with-element-type vector
    (for-each-relevant-element (e index vector start end nil)
      (declare (ignore e))
      (setf (aref vector index) item))))
