(cl:in-package #:sicl-package)

;;; FIXME: it should be the CL-USER package.
(defparameter *package* nil)

(defun export (symbols-designator &optional package-designator *package*)
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          do (export-one-symbol symbol package)))
  t)
