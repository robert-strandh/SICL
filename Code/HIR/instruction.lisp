(cl:in-package #:sicl-hir)

(defclass instruction ()
  ((%predecessors
    :initform '()
    :initarg :predecessors
    :accessor predecessors)
   (%successors
    :initform '()
    :initarg :successors
    :accessor successors)
   (%inputs
    :initform '()
    :initarg :inputs
    :accessor inputs)
   (%outputs
    :initform '()
    :initarg :outputs
    :accessor outputs)))
