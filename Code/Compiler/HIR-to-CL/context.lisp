(cl:in-package #:sicl-hir-to-cl)

(defclass context ()
  ((%visited :initform (make-hash-table :test #'eq) :reader visited)))
