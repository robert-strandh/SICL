(cl:in-package #:sicl-hir-to-cl)

(defclass context ()
  ((%visited :initform (make-hash-table :test #'eq) :reader visited)
   (%tags :initform (make-hash-table :test #'eq) :reader tags)
   (%function-names :initform (make-hash-table :test #'eq) :reader function-names)
   (%values-location :initform (gensym "values") :reader values-location)
   (%block-name :initform (gensym "block") :reader block-name)
   (%static-env-function-var
    :initform (gensym "static-env-function")
    :reader static-env-function-var)))

(defvar *static-environment-variable*)

(defvar *top-level-function-parameter*)

(defvar *dynamic-environment-staack* '())
