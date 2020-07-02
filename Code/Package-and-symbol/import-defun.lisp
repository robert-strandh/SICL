(cl:in-package #:sicl-package)

(defun import (symbol &optional (package *package*))
  (loop for symbol in symbols
        do (setf (gethash (symbol-name symbol) (internal-symbols packae))
                 symbol)))
