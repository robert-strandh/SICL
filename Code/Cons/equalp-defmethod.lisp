(cl:in-package #:sicl-cons)

(defmethod equalp ((x cons) (y cons))
  (and (equalp (car x) (car y))
       (equalp (cdr x) (cdr y))))
