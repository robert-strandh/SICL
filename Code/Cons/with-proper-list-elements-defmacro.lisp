(cl:in-package #:sicl-cons)

(defmacro with-proper-list-elements ((element-var list function-name) &body body)
  (let ((list-var (gensym))
        (rest-var (gensym)))
    `(loop with ,list-var = ,list
           for ,rest-var on ,list-var
           for ,element-var = (car ,rest-var)
           do ,@body
           finally (unless (null ,rest-var)
                     (error 'must-be-proper-list
                            :datum ,list-var
                            :name ,function-name)))))
