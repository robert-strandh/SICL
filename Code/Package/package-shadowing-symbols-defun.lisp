(cl:in-package #:sicl-package)

(defun package-shadowing-symbols (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (copy-list (shadowing-symbols package))))
