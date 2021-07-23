(cl:in-package #:sicl-package)

(defun import-one-symbol (symbol package)
  (setf (gethash (symbol-name symbol) (internal-symbols package))
        symbol)))

(defun import (symbols &optional (package-designator *package*))
  (loop with package = (package-designator-to-package package-designator)
        for symbol in symbols
        do (import-one-symbol symbol package)))
