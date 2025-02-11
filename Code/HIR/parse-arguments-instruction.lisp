(cl:in-package #:sicl-hir)

;;; This instruction has only outputs.  It has one output for each
;;; required parameter.  It has two outputs for each &OPTIONAL
;;; parameter, one for the parameter itself and one for the SUPPLIED-P
;;; parameter.  If the lambda list contains &REST, then this
;;; instruction has one output for that parameter.  It has two outputs
;;; for each &KEY parmeter one for the parameter itself and one for
;;; the SUPPLIED-P parameter.

(defclass parse-arguments-instruction (instruction)
  ((%lambda-list
    :initarg :lambda-list
    :reader lambda-list)))

