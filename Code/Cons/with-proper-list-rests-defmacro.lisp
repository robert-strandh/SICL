(cl:in-package #:sicl-cons)

(defmacro with-proper-list-rests ((rest-var list function-name) &body body)
  (let ((list-var (gensym)))
    `(loop with ,list-var = ,list
           for ,rest-var = ,list-var then (rest ,rest-var)
           until (atom ,rest-var)
           do ,@body
           finally (unless (null ,rest-var)
                     (error 'must-be-proper-list
                            :datum ,list-var
                            :name ,function-name)))))
