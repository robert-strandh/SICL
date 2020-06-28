(cl:in-package #:sicl-sequence)

(defmacro for-each-relevant-element
    ((element-var index-var vector start end &optional (from-end nil)) &body body)
  (let* ((vector-var (gensym))
         (start-var (gensym))
         (end-var (gensym))
         (forward
           `(loop for ,index-var of-type fixnum from ,start-var below ,end-var
                  do (symbol-macrolet ((,element-var (elt ,vector-var ,index-var)))
                       ,@body)))
         (backward
           `(loop for ,index-var of-type fixnum downfrom (1- ,end-var) to ,start-var
                  do (symbol-macrolet ((,element-var (elt ,vector-var ,index-var)))
                       ,@body)))
         (loop-over-body
           (cond ((eql from-end nil) forward)
                 ((eql from-end t) backward)
                 (t `(if ,from-end ,backward ,forward)))))
    `(let ((,vector-var ,vector))
       (with-vector-start-and-end (,start-var ,end-var) (,vector-var ,start ,end)
         (declare (type fixnum ,start-var ,end-var))
         ,loop-over-body))))
