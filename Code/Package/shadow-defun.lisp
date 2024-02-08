(cl:in-package #:sicl-package)

(defun shadow (symbol-names &optional (package-designator *package*))
  (loop with package = (package-designator-to-package package-designator)
        for name in symbol-names
        do (parcl:shadow env:*client* package name)))
