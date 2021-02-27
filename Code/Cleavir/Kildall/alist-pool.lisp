(in-package #:cleavir-kildall)

(defclass alist-pool-mixin () ())

;;; move? not sure
(defgeneric instruction-variables (specialization instruction))

(defmethod make-pool
    ((specialization alist-pool-mixin) instruction)
  (loop for variable
          in (instruction-variables specialization instruction)
        collect (cons variable (object1 specialization variable))))

(defmethod find-in-pool
    ((specialization alist-pool-mixin) variable pool)
  (let ((pair (assoc variable pool :test #'eq)))
    (if (null pair)
        (object1 specialization variable)
        (cdr pair))))

(defmethod map-into-pool
    ((specialization alist-pool-mixin) function pool)
  (loop for pair in pool
        do (setf (cdr pair)
                 (funcall function (car pair) (cdr pair)))))
