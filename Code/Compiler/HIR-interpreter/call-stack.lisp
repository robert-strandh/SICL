(cl:in-package #:sicl-hir-interpreter)

(defparameter *call-stack* '())

(defclass call-stack-entry ()
  ((%origin :initarg :origin :reader origin)
   (%arguments :initarg :arguments :reader arguments)))
