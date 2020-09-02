(cl:in-package #:sicl-conditions)

(defmacro handler-bind (bindings &body forms)
  (flet ((make-binding (binding)
           (destructuring-bind (condition-type function) binding
             `(cons ',condition-type ,function))))
    (let ((cluster (mapcar #'make-binding bindings)))
      `(let ((*handler-clusters* (cons (list ,@cluster) *handler-clusters*)))
         ,@forms))))
