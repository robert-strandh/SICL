(cl:in-package #:sicl-future-cst-to-ast)

(defclass builder (bld:builder)
  ((%environment
    :initarg :environment
    :reader environment)))
