(cl:in-package #:sicl-clos)

(defun same-specializers-p (parameter-specializers specializers)
  (and (= (length parameter-specializers) (length specializers))
       (loop for parameter-specializer in parameter-specializers
             for specializer in specializers
             unless (or (and (typep parameter-specializer 'class)
                             (eq parameter-specializer specializer))
                        (and (typep specializer 'eql-specializer)
                             (eq (second parameter-specializer)
                                 (eql-specializer-object specializer))))
               return nil
             finally (return t))))

(defmethod find-method
    ((generic-function standard-generic-function)
     method-qualifiers
     parameter-specializers
     &optional
       errorp)
  (loop for parameter-specializer in parameter-specializers
        unless (or (typep parameter-specializer 'class)
                   (and (consp parameter-specializer)
                        (eq (first parameter-specializer) 'eql)
                        (consp (rest parameter-specializer))
                        (null (rest (rest parameter-specializer)))))
          do (error 'illegal parameter specializer
                    :datum parameter-specializer
                    :expected-type '(or method (cons (eql eql) (cons * null)))))
  (loop for method in (generic-function-methods generic-function)
        for specializers = (method-specializers method)
        when (and (equal method-qualifiers (method-qualifiers method))
                  (= (length parameter-specializers) (length specializers))
                  (same-specializers-p parameter-specializers specializers))
          return method
        finally (if errorp
                    (error 'no-method-matches-qualifiers-and-specializers)
                    (return nil))))
                   
                   
