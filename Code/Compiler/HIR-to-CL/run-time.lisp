(cl:in-package #:sicl-hir-to-cl)

;;; Most of the code in this file should be moved to the boot system
;;; once everything is working.

(defgeneric static-environment (function))

(defclass funcallable-standard-class (closer-mop:funcallable-standard-class)
  ((%static-environment :initarg :static-environment :reader static-environment)))
