(cl:in-package #:sicl-sequence)

(defmacro for-each-relevant-element
    ((element-var index-var sequence start end &optional (from-end nil from-end-p)) &body body)
  (let* ((sequence-var (gensym))
         (start-var (gensym))
         (end-var (gensym))
         (forward
           `(loop for ,index-var of-type fixnum from ,start-var below ,end-var
                  do (symbol-macrolet ((,element-var (elt ,sequence-var ,index-var)))
                       ,@body)))
         (backward
           `(loop for ,index-var of-type fixnum downfrom (1- ,end-var) to ,start-var
                  do (symbol-macrolet ((,element-var (elt ,sequence-var ,index-var)))
                       ,@body)))
         (loop-over-body
           (if (not from-end-p)
               forward
               `(if ,from-end ,backward ,forward))))
    `(let ((,sequence-var ,sequence))
       (multiple-value-bind (,start-var ,end-var)
           (canonicalize-start-and-end ,sequence-var (length ,sequence-var) ,start ,end)
         (declare (type fixnum ,start-var ,end-var))
         ,loop-over-body))))
