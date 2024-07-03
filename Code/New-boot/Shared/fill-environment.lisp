(cl:in-package #:sicl-new-boot)

(defun fill-environment (client global-environment)
  (loop for symbol being each symbol
          in (find-package '#:common-macro-definitions)
        unless (or (null (macro-function symbol))
                 (eq (cl:symbol-package symbol) (find-package "CL"))
                 (string= (cl:symbol-name symbol) "DEFMACRO"))
          do (let ((name (find-symbol (string-upcase (cl:symbol-name symbol))
                                      (find-package "CL"))))
               (setf (clo:macro-function client global-environment name)
                     (macro-function symbol))))
  (loop for symbol being each external-symbol
          in (find-package '#:common-lisp)
        when (special-operator-p symbol)
          do (clo:make-special-operator client global-environment symbol t)))
