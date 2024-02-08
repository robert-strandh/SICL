(cl:in-package #:sicl-package)

(defun import (symbols-designator &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          do (parcl:import env:*client * package symbol))))
