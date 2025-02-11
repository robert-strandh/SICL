(cl:in-package #:sicl-hir)

(defclass instruction ()
  ((%predecessors
    :initarg :predecessors
    :accessor predecessors)
   (%successors
    :initarg :successors
    :accessor successors)
   (%inputs
    :initarg :inputs
    :accessor inputs)
   (%outputs
    :initarg :outputs
    :accessor outputs)))
