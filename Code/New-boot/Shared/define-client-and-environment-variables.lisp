(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

(defun define-client-and-environment-variables (client environment)
  (clo:make-variable client environment
                     @sicl-environment:*environment*
                     environment)
  (clo:make-variable client environment
                     @sicl-environment:*client*
                     client))
