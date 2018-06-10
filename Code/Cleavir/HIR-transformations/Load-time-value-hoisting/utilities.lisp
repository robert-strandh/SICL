(cl:in-package #:cleavir-load-time-value-hoisting)

(defmacro with-memoization ((object hash-table) &body body)
  (let ((key (gensym))
        (table (gensym))
        (value (gensym))
        (present-p (gensym)))
    `(let ((,key ,object)
           (,table ,hash-table))
       (multiple-value-bind (,value ,present-p)
           (gethash ,key ,table)
         (if ,present-p ,value
             (setf (gethash ,key ,table)
                   (progn ,@body)))))))
