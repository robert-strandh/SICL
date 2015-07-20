(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-METHOD-COMBINATION.

(defclass simple-method-combination (method-combination)
  ((%operation :initarg :operation :reader operation)))
