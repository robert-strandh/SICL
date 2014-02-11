(defun make-reader-method-function (slot-name)
  (lambda (arguments next-methods)
    (declare (ignore next-methods))
    (slot-value (car arguments) slot-name)))
