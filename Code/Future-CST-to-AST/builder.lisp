(cl:in-package #:sicl-future-cst-to-ast)

(defgeneric environment (builder))

(defclass builder (bld:builder)
  ((%environment
    :initarg :environment
    :reader environment)))
