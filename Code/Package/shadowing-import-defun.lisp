(cl:in-package #:sicl-package)

(defun shadowing-import (symbols-designator &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          do (parcl:shadowing-import env:*client* package symbol))))
