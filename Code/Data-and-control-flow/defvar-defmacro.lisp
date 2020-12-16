(cl:in-package #:sicl-data-and-control-flow)

(defmacro defvar
    (&environment environment name &optional initial-value documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  (let ((env-var (gensym))
        (client-var (gensym)))
    `(progn
       (eval-when (:compile-toplevel)
         (let* ((,env-var (sicl-environment:global-environment ,environment))
                (,client-var (sicl-environment:client ,env-var)))
           (setf (sicl-environment:variable-description
                  ,client-var ,env-var ',name)
                 (make-instance 'sicl-environment:special-variable-description))))
       (eval-when (:load-toplevel :execute)
         (setf (special-variable ',name nil) nil)
         (unless (boundp ',name)
           (setf (symbol-value ',name) ,initial-value))
         ',name))))
