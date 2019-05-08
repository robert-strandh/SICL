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

;;; When a single function is translated, the value of this special
;;; variable is an EQ hash table.  Each key in that hash table is a
;;; lexical location of that single function, corresponding to a
;;; dynamic environment.  The value is a list of basic blocks of that
;;; function that are executed in the corresponding dynamic
;;; environment at run time.
(defvar *basic-blocks-in-dynamic-environment*)

;;; Given a lexical location corresponding to a dynamic environment,
;;; return a list of basic blocks that will be executed in the
;;; corresponding dynamic environment at run time.
(defun basic-blocks-in-dynamic-environment (dynamic-environment-location)
  (gethash dynamic-environment-location *basic-blocks-in-dynamic-environment*))

;;; When a single function is translated, the value of this special
;;; variable is n EQ hash table.  Each key in that hash table is a
;;; LEADER, i.e. the first instruction of some basic block.  The value
;;; is the basic block having that leader.
(defvar *basic-block-of-leader*)

;;; Given a LEADER, i.e. the first instruction of some basic block,
;;; return the basic block having that instruction as its leader.
(defun basic-block-of-leader (leader)
  (gethash leader *basic-block-of-leader*))
