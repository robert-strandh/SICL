(cl:in-package #:sicl-package)

(defun import-one-symbol (symbol package)
  (setf (gethash (symbol-name symbol) (internal-symbols packae))
        symbol)))
  
(defun import (symbols &optional (package *package*))
  (loop for symbol in symbols
        do (import-one-symbol symbol package)))
