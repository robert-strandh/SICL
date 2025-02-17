(cl:in-package #:sicl-ast-to-hir)

(defvar *next-instruction*)

;;; The target register is either NIL (meaning that no values are
;;; required), an instance of SINGLE-VALUE-REGISTER (indicating that a
;;; single value is required), or an instance of
;;; MULTIPLE-VALUE-REGISTER (indicating that all values are required).
(defvar *target-register*)

(defvar *dynamic-environment-register*)

(defvar *static-environment-register*)
