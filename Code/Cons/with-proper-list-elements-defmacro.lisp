(cl:in-package #:sicl-cons)

(defmacro with-proper-list-elements ((element-var list function-name) &body body)
  (let ((list-var (gensym))
        (rest-var (gensym)))
    `(loop with ,list-var = ,list
           for ,rest-var = ,list-var then (rest ,rest-var)
           for ,element-var = (if (atom ,rest-var) nil (first ,rest-var))
           until (atom ,rest-var)
           do ,@body
           finally (unless (null ,rest-var)
                     (error 'must-be-proper-list
                            :datum ,list-var
                            :name ,function-name)))))
