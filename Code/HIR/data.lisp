(cl:in-package #:sicl-hir)

(defclass register ()
  ((%readers
    :initarg :readers
    :accessor readers)
   (%writers
    :initarg :writers
    :accessor writers)))

(defclass literal ()
  ((%readers
    :initarg :readers
    :accessor readers)))
