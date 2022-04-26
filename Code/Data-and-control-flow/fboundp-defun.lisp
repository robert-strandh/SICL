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

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (fdefinition (fdefinition 'sicl-environment:fdefinition))
       (special-operator (fdefinition 'sicl-environment:special-operator))
       (macro-function (fdefinition 'sicl-environment:macro-function)))
  (defun fboundp (function-name)
    (or (not (null (funcall fdefinition client environment function-name)))
        (and (symbolp function-name)
             (or (not (null (funcall special-operator client environment function-name)))
                 (not (null (funcall macro-function client environment function-name))))))))
