(cl:in-package #:sicl-clos)

(defmacro with-accessors ((&rest slot-entries) instance-form &rest body)
  (let ((instance-var (gensym)))
    `(let ((,instance-var ,instance-form))
       (symbol-macrolet
           ,(loop for (variable accessor) in slot-entries
                  collect `(,variable (,accessor ,instance-var)))
         ,@body))))
