(cl:in-package #:sicl-package)

(defun find-symbol (symbol-name &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (parcl:find-symbol env:*client* package symbol-name)))
