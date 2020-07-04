;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; COERCE-TO-CONDITION

(defgeneric coerce-to-condition (datum arguments default-type name)
  (:documentation ))

(defmethod coerce-to-condition ((datum condition) arguments default-type name)
  (when arguments
    (cerror "Ignore the additional arguments."
            'simple-type-error
            :datum arguments
            :expected-type 'null
            :format-control
            "You may not supply additional arguments when giving ~S to ~S."
            :format-arguments (list datum name)))
  datum)

(defmethod coerce-to-condition ((datum symbol) arguments default-type name)
  (apply #'make-condition datum arguments))

(defmethod coerce-to-condition ((datum string) arguments default-type name)
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition ((datum function) arguments default-type name)
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition (datum arguments default-type name)
  (error 'simple-type-error
         :datum datum
         :expected-type '(or condition symbol function string)
         :format-control "~S is not coercable to a condition."
         :format-arguments (list datum)))
