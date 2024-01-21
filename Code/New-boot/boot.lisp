(cl:in-package #:sicl-new-boot)

(defun boot ()
  (let ((boot (make-instance 'boot))
        (client (make-instance 'client))
        (*packages* (make-hash-table :test #'equal))
        (*symbol-package* (make-hash-table :test #'eq)))
    (create-common-lisp-package client)
    (loop for name in '("COMMON-LISP-USER" "KEYWORD")
          do (setf (gethash name *packages*) (find-package name)))
    (values boot *packages* *symbol-package*
            (sicl-new-boot-phase-1:boot boot))))
