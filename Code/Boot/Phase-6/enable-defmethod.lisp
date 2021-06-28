(cl:in-package #:sicl-boot-phase-6)

(defun enable-defmethod (e5)
  (load-source-file "CLOS/add-remove-direct-method.lisp" e5)
  (load-source-file "CLOS/dependent-maintenance.lisp" e5)
  (load-source-file "CLOS/add-remove-method.lisp" e5))
