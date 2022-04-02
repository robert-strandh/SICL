(cl:in-package #:sicl-data-and-control-flow)

(defmacro defconstant
    (&environment environment name initial-value &optional documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  (let ((env-var (gensym)))
    `(progn
       (eval-when (:compile-toplevel)
         (let* ((,env-var (sicl-environment:global-environment ,environment)))
           (setf (sicl-environment:variable-description ,env-var ',name)
                 (sicl-environment:make-constant-variable-description initial-value))))
       (eval-when (:load-toplevel :execute)
         (setf (constant-variable ',name) ,initial-value)))))
