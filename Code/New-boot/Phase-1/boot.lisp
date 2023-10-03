(cl:in-package #:sicl-new-boot-phase-1)

(defun boot ()
  (let* ((client (make-instance 'client))
         (environment (create-environment))
         (global-environment (trucler:global-environment client environment))
         (*packages* (make-hash-table :test #'equal))
         (*symbol-package* (make-hash-table :test #'eq)))
    (import-from-host client global-environment)
    (define-environment-functions client global-environment)
    (define-package-functions client global-environment)
    (clostrum:make-variable
     client global-environment '*package* (find-package '#:common-lisp))
    (load-file client "to-delete.lisp" environment)))
