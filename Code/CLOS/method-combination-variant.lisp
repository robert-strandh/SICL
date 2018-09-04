(cl:in-package #:sicl-clos)

(defclass method-combination-variant ()
  ((%options :initarg :options :reader options)
   (%expander :initarg :expander :reader expander)))
