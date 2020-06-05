(cl:in-package #:sicl-sequence)

(defmethod sort ((list list) predicate &key key)
  (stable-sort list predicate :key key))
