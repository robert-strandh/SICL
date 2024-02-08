(cl:in-package #:sicl-package)

(defun unintern (symbol &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (parcl:unintern env:*client* package symbol)))
