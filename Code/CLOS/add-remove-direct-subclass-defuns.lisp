(cl:in-package #:sicl-clos)

(defun add-direct-subclass (superclass subclass)
  (add-direct-subclass-default superclass subclass))

(defun remove-direct-subclass (superclass subclass)
  (remove-direct-subclass-default superclass subclass))
