(cl:in-package #:sicl-new-boot)

(defun boot ()
  (let* ((boot (make-instance 'boot))
         (*boot* boot)
         (client (make-instance 'client)))
    (create-common-lisp-package client)
    (loop for name in '("COMMON-LISP-USER" "KEYWORD")
          do (setf (gethash name (packages boot))
                   (find-package name)))
    (sicl-new-boot-phase-1:boot boot)
    (sicl-new-boot-phase-2:boot boot)
    (sicl-new-boot-phase-3:boot boot)
    (defparameter *b* boot)))
