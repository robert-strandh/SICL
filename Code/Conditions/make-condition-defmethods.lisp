(cl:in-package #:sicl-conditions)

(defmethod make-condition
    ((type symbol) &rest slot-initializations)
  (apply #'make-condition
         (find-class type)
         slot-initializations))

(defmethod make-condition
    ((type condition-class) &rest slot-initializations)
  (apply #'make-instance type slot-initializations))

(defmethod make-condition
    (type &rest slot-initializations)
  (error 'type-error
         :datum type
         :expected-type '(or condition-class)))
