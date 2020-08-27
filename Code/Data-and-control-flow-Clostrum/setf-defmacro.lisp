(cl:in-package #:sicl-data-and-control-flow)

(defmacro setf (&whole form &environment env place new-value-form &rest more-pairs)
  (cond ((null more-pairs)
         (multiple-value-bind (variables
                               values
                               store-variables
                               writer-form
                               reader-form)
             (get-setf-expansion place env)
           (declare (ignore reader-form))
           `(let* ,(mapcar #'list variables values)
              ;; Optimize a bit when there is only one store variable.
              ,(if (= 1 (length store-variables))
                   `(let ((,(first store-variables) ,new-value-form))
                      ,writer-form)
                   `(multiple-value-bind ,store-variables
                        ,new-value-form
                      ,writer-form)))))
        ((not (null (cdr more-pairs)))
         `(progn (setf ,place ,new-value-form)
                 (setf ,@more-pairs)))
        (t
         (error 'odd-number-of-arguments-to-setf :form form))))
