(cl:in-package #:sicl-hir)

(defclass register ()
  ((%readers
    :initarg :readers
    :accessor readers)
   (%writers
    :initarg :writers
    :accessor writers)))

(defclass single-value-register (register)
  ())

(defclass multiple-value-register (register)
  ())

(defclass literal ()
  ((%readers
    :initarg :readers
    :accessor readers)))
