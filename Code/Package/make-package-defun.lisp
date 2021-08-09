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
         (all-name-strings (cons name-string nickname-strings))
         (existing-packages
           (loop for name in all-name-strings
                 for package = (find-package name)
                 unless (null package)
                   collect package)))
    (loop until (null existing-packages)
          do (restart-case (error 'package-already-exists
                                  :packages existing-packages)
               (force ()
                 :report (lambda (stream)
                           (format stream
                                   "Replace the existing packages."))
                 (mapc #'delete-package existing-packages)
                 (setf existing-packages '()))))
    (let ((package (make-instance 'package
                     :name name-string
                     :nicknames nickname-strings
                     ;; FIXME: create with a use list of '() and then
                     ;; import and check for errors.
                     :use-list use)))
      (loop for name in all-name-strings
            do (setf (find-package name) package))
      package)))
