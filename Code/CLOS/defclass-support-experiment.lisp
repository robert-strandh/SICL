(cl:in-package #:sicl-clos)

;;; This file contains an experiment that uses the s-expression-syntax
;;; library to parse DEFCLASS.

(defclass defclass-form ()
  (%name :accessor name)
  (%superclasses :initform '() :accessor superclasses)
  (%slot-descriptions :initform '() :accessor slot-descriptions)
  (%default-initform
