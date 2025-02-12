(cl:in-package #:sicl-hir)

;;; This instruction has one input and two output.  The input is a
;;; dynamic-environment object representing the cyrrent dynamic
;;; environment.  The first output is a dynamic-environment object
;;; which is like the input but with a new exit point added.  The
;;; second output is a unique identifier for this exit point.  The
;;; unique identifier is used by the UNWIND-INSTRUCTION to find the
;;; unique exit point where control is to be transferred.

(defclass exit-point-instruction (instruction)
  ())
