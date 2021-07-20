(cl:in-package #:sicl-cons)

(defmethod equal ((x cons) (y cons))
  (and (equal (car x) (car y))
       (equal (cdr x) (cdr y))))
