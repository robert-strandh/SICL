(cl:in-package #:sicl-string)

(defclass string
    (sicl-vector:vector sicl-array:array-character)
  ())
