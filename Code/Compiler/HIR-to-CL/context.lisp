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

;;; When a single function is translated, this special variable holds
;;; an EQ hash table.  Each key in that hash table is a lexical
;;; location of that single function, corresponding to a dynamic
;;; environment.  The value is a list of basic blocks of that function
;;; that are executed in the corresponding dynamic environment at run
;;; time.
(defvar *basic-blocks-in-dynamic-environment*)

;;; Given a lexical location corresponding to a dynamic environment,
;;; return a list of basic blocks that will be executed in teh
;;; corresponding dynamic environment at run time.
(defun basic-blocks-in-dynamic-environment (dynamic-environment-location)
  (gethash dynamic-environment-location *basic-blocks-in-dynamic-environment*))
