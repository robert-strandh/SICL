(cl:in-package #:sicl-package)

(defun use-package
    (designators-of-packages-to-use &optional package-designator)
  (when (atom designators-of-packages-to-use)
    (setf designators-of-packages-to-use
          (list designators-of-packages-to-use)))
  (unless (proper-list-p designators-of-packages-to-use)
    (error 'use-list-must-be-proper-list
           :datum designators-of-packages-to-use))
  (let ((packages-to-use
          (mapcar #'package-designator-to-package
                  designators-of-packages-to-use))
        (package (package-designator-to-package package-designator)))
    (parcl:use-packages env:*client* package packages-to-use)))
