(cl:in-package #:sicl-package)

(defun make-package (name &key nicknames use)
  (unless (proper-list-p nicknames)
    (error 'nicknames-must-be-proper-list
           :datum nicknames))
  (unless (proper-list-p use)
    (error 'use-list-must-be-proper-list
           :datum use))
  (let* ((name-string (string name))
         (nickname-strings (mapcar #'string nicknames))
         (used-packages (mapcar #'package-designator-to-package use))
         (result (parcl:make-package env*client* name-string)))
    (setf (parcl:nicknames env:*client* result) nickname-strings)
    (loop for name-string in (cons name-string nickname-strings)
          do (setf (env:find-package name-string) result))
    result))
