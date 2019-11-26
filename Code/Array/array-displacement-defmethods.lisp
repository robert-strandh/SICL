(cl:in-package #:sicl-array)

(defmethod array-displacement (object)
  ;; FIXME: signal a more specific condition
  (error 'type-error :datum object :expected-type 'array))

(defmethod array-displacement ((array array))
  (values nil 0))

(defmethod array-displacement ((array displaced-array))
  (values (target array) (offset array)))
