(cl:in-package #:sicl-array)

(defgeneric adjustable-array-p (array))

(defmethod adjustable-array-p (array)
  (error 'type-error
         :datum array
         :expected-type 'array))

;;; FIXME: Are displaced arrays adjustable?
(defmethod adjustable-array-p ((array array))
  t)
