(cl:in-package #:cleavir-load-time-value-hoisting)

(defmacro with-memoization ((object hash-table) value-form &body body)
  "Unless OBJECT is found in HASH-TABLE, evaluate VALUE-FORM, store the
result as the value of OBJECT in HASH-TABLE and evaluate BODY."
  (let ((key (gensym))
        (table (gensym))
        (value (gensym))
        (present-p (gensym)))
    `(let ((,key ,object)
           (,table ,hash-table))
       (multiple-value-bind (,value ,present-p)
           (gethash ,key ,table)
         (declare (ignore ,value))
         (unless ,present-p
           (setf (gethash ,key ,table) ,value-form)
           ,@body)))))
