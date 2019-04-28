(cl:in-package #:sicl-hir-to-cl)

(defclass context ()
  ((%visited :initform (make-hash-table :test #'eq) :reader visited)
   (%values-location :initform (gensym) :reader values-location)
   (%block-name :initform (gensym) :reader block-name)))
