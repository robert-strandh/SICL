(cl:in-package #:cleavir-ast)

;;;; We define the abstract syntax trees (ASTs) that represent not
;;;; only Common Lisp code, but also the low-level operations that we
;;;; use to implement the Common Lisp operators that can not be
;;;; portably implemented using other Common Lisp operators.
;;;; 
;;;; The AST is a very close representation of the source code, except
;;;; that the environment is no longer present, so that there are no
;;;; longer any different namespaces for functions and variables.  And
;;;; of course, operations such as MACROLET are not present because
;;;; they only alter the environment.  
;;;;
;;;; The AST form is the preferred representation for some operations;
;;;; in particular for PROCEDURE INTEGRATION (sometimes called
;;;; INLINING).

(defgeneric children (ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variable *DYNAMIC-ENVIRONMENT*.
;;; Default for :dynamic-environment initarg.

(defvar *dynamic-environment*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AST.  The base class for all AST classes.
;;;
;;; DYNAMIC-ENVIRONMENT is a lexical-ast representing the dynamic
;;; environment in force.

(defclass ast ()
  ((%dynamic-environment :initform *dynamic-environment*
                         :initarg :dynamic-environment
                         :accessor dynamic-environment)))

;;; Policies must be saved
(cleavir-io:define-save-info ast
  (:dynamic-environment dynamic-environment))

