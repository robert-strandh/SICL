(cl:in-package #:cleavir-value-hoisting-test)

(defmethod cleavir-value-hoisting:make-load-form-using-client
    ((client client) (string string) environment)
  (values
   `(%make-string
     ,@(loop for index below (length string)
             collect (row-major-aref string index)))))

(defmethod cleavir-value-hoisting:make-load-form-using-client
    ((client client) (package package) environment)
  `(%find-package ,(package-name package)))

(defmethod cleavir-value-hoisting:make-load-form-using-client
    ((symbol symbol) (client client) environment)
  `(%intern ,(symbol-name symbol)
            ,(symbol-package symbol)))

(defmethod cleavir-value-hoisting:make-load-form-using-client
    ((cons cons) (client client) environment)
  (values
   `(%cons)
   `(setf (car ,cons) ',(car cons)
          (cdr ,cons) ',(cdr cons))))
