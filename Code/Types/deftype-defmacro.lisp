(cl:in-package #:sicl-type)

(defmacro deftype (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name)
           (function ,(cleavir-code-utilities:parse-deftype 
                       name
                       lambda-list
                       body)))))
