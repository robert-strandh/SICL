(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun define-find-method-combination-template (client e2)
  (setf (clo:fdefinition
         client e2 @clostrophilia:find-method-combination-template)
        (lambda (client name)
          (declare (ignore client))
          (sicl-environment:find-method-combination-template name e2))))
