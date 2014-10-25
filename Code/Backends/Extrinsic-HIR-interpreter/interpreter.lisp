(cl:in-package #:sicl-extrinsic-hir-interpreter)

(defclass function () ())

(defclass built-in-function (function)
  ((%host-function :initarg :host-function :reader host-function)))

(defclass interpreted-function (function)
  (;; The initial instruction of the function.  It is always an
   ;; ENTER-INSTRUCTION.
   (%entry-point :initarg :entry-point :reader entry-point)
   ;; The captured static environment.
   (%environment :initarg :environment :reader environment)
   ;; A list of lexical variables that should be used to augment the
   ;; captured environment when the function is called.
   (%entry-lexicals :initarg :entry-lexicals :reader entry-lexicals)))
