(cl:in-package #:sicl-package)

(defun intern (symbol-name &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (parcl:intern env:*client* package symbol-name)))
