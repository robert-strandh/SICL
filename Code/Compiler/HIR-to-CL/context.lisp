(cl:in-package #:sicl-hir-to-cl)

(defclass context ()
  ((%visited :initform (make-hash-table :test #'eq) :reader visited)
   (%function-names :initform (make-hash-table :test #'eq) :reader function-names)
   (%values-location :initform (gensym "values") :reader values-location)
   (%block-name :initform (gensym "block") :reader block-name)
   (%enclose-function-var
    :initform (gensym "enclose-function")
    :reader enclose-function-var)
   (%make-cell-function-var
    :initform (gensym "make-cell-function")
    :reader make-cell-function-var)
   (%fetch-function-var
    :initform (gensym "fetch-function")
    :reader fetch-function-var)
   (%static-env-function-var
    :initform (gensym "static-env-function")
    :reader static-env-function-var)))
