(cl:in-package #:sicl-data-and-control-flow)

(defmacro defconstant
    (&environment environment name initial-value &optional documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  (let ((env-var (gensym)))
    `(progn
       (eval-when (:compile-toplevel)
         (let* ((,env-var (env:global-environment ,environment)))
           (setf (env:variable-description ,env-var ',name)
                 (env:make-constant-variable-description initial-value))))
       (eval-when (:load-toplevel :execute)
         (setf (constant-variable ',name) ,initial-value)))))
