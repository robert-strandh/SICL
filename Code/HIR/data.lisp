(cl:in-package #:sicl-hir)

(defclass datum ()
  ((%readers
    :initform '()
    :initarg :readers
    :accessor readers)))

(defclass register (datum)
  ((%writers
    :initform '()
    :initarg :writers
    :accessor writers)))

(defclass single-value-register (register)
  ())

(defclass multiple-value-register (register)
  ())

(defclass literal (datum)
  ((%value
    :initarg :value
    :reader value)))
