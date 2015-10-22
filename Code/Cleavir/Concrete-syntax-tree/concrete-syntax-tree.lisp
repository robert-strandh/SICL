(cl:in-package #:cleavir-cst)

(defclass cst ()
  ((%expression :initarg :expression :reader expression)
   (%location :initarg :location :reader location)
   (%children :initarg :children :reader children)))
