(cl:in-package #:sicl-conditions)

(defmacro restart-bind (bindings &body body)
  (let ((cluster (mapcar #'restart-bind-transform-binding bindings)))
    `(let ((*restart-clusters* (cons (list ,@cluster) *restart-clusters*)))
       ,@body)))
