(cl:in-package #:sicl-new-boot-phase-2)

(defun define-find-method-combination-template (client e2)
  (let ((symbol
          (sb:intern-parcl-symbol
           client "CLOSTROPHILIA" "FIND-METHOD-COMBINATION-TEMPLATE")))
    (setf (clo:fdefinition client e2 symbol)
          (lambda (client name)
            (declare (ignore client))
            (sicl-environment:find-method-combination-template name e2)))))
