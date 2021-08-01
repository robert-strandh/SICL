(cl:in-package #:sicl-package)

(defun shadowing-import (symbols-designator &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          for name = (symbol-name symbol)
          for cell = (member name (shadowing-symbols package)
                             :key #'symbol-name
                             :test #'equal)
          do (remhash name (external-symbols package))
             (remhash name (internal-symbols package))
             (setf (gethash name (external-symbols package))
                   symbol)
             (setf (gethash name (internal-symbols package))
                   symbol)
             (unless (null cell)
               (setf (car cell) symbol)))))
