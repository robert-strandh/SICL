(cl:in-package #:sicl-package)

(defun package-used-by-list (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (copy-list (used-by-list package))))
