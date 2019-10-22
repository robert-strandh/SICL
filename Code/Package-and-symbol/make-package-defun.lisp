(cl:in-package #:sicl-package)

;;; FIXME: signal correctable errors.
;;; FIXME: check that nicknames is a proper list of string designators
;;; FIXME: check that use is a proper list of package designators.
(defun make-package (name &key nicknames use)
  (declare (type string-designator name))
  (let ((existing-package (find-package name)))
    (loop until (null existing-package)
          do (restart-case (error 'package-already-exists
                                  :name existing-package)
               (force (stuff)
                 :report (lambda (stream)
                           (format stream
                                   "Replace the existing package."))
                 (setf existing-package nil))
               (change-name (new-package-name)
                 :report (lambda (stream)
                           (format stream
                                   "Make a package with a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setf existing-package (find-package new-package-name))))))
  (loop for rest = nicknames then (cdr rest)
	while (consp rest)
	do (assert (null (find-package (car rest)))))
  (let ((package (make-instance 'package)))
    (setf (name package) (string name))
    (setf (nicknames package) (copy-list nicknames))
    (use-package use package)
    package))
