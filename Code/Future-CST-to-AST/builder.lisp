(cl:in-package #:sicl-future-cst-to-ast)

(defgeneric environment (builder))

(defgeneric client (builder))

(defclass builder (bld:builder)
  ((%environment
    :initarg :environment
    :reader environment)
   (%client
    :initarg :client
    :reader client)))
