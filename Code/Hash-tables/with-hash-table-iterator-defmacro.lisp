(cl:in-package #:sicl-hash-table)

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((iterator-var (gensym)))
    `(let ((,iterator-var (make-hash-table-iterator ,hash-table)))
       (macrolet ((,name () (funcall ,iterator-var)))
         ,@body))))
