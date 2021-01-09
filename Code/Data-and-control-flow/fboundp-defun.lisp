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
       (fboundp (fdefinition 'sicl-environment:fboundp)))
  (defun fboundp (function-name)
    (funcall fboundp client environment function-name)))
