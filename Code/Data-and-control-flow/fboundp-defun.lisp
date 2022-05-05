(cl:in-package #:sicl-data-and-control-flow)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FBOUNDP.
;;;
;;; According to the HyperSpec, this function should return any true
;;; value if the name is fbound in the global environment.  From the
;;; glossary, we learn that "fbound" means that the name has a
;;; definition as either a function, a macro, or a special operator in
;;; the global environment.

(symbol-macrolet ((client sicl-client:*client*))
  (let* ((environment (env:global-environment))
         (fdefinition (fdefinition 'env:fdefinition))
         (special-operator (fdefinition 'env:special-operator))
         (macro-function (fdefinition 'env:macro-function)))
    (defun fboundp (function-name)
      (or (not (null (funcall fdefinition client environment function-name)))
          (and (symbolp function-name)
               (or (not (null (funcall special-operator client environment function-name)))
                   (not (null (funcall macro-function client environment function-name)))))))))
