(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-OBJECT-P-AST.
;;;
;;; This AST can be used by implementations that identify standard
;;; objects by pointer tags.  It can only occur as the test of an
;;; IF-AST.

(defclass standard-object-p-ast (ast boolean-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info standard-object-p-ast
  (:object-ast object-ast))

(defmethod children ((ast standard-object-p-ast))
  (list (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-OBJECT-CLASS-OF-AST.
;;;
;;; This AST can be used to compute the class of a standard object.
;;; The child AST must evaluate to a standard object.

(defclass standard-object-class-of-ast (ast one-value-ast-mixin)
  ((%standard-object-ast :initarg :standard-object-ast
                         :reader standard-object-ast)))

(cleavir-io:define-save-info standard-object-class-of-ast
  (:standard-object-ast standard-object-ast))

(defmethod children ((ast standard-object-class-of-ast))
  (list (standard-object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NOOK-READ-AST.
;;;
;;; This AST can be used to read a nook from a standard object.  It
;;; has two children, an AST that must have a standard object as its
;;; value, and an AST that must have a fixnum as its value and that
;;; indicates a nook number (starting from 0).  This AST generates a
;;; single value, namely the contents of the nook with the number given.

(defclass nook-read-ast (ast one-value-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%nook-number-ast :initarg :nook-number-ast :reader nook-number-ast)))

(cleavir-io:define-save-info nook-read-ast
  (:object-ast object-ast)
  (:nook-number-ast nook-number-ast))

(defmethod children ((ast nook-read-ast))
  (list (object-ast ast) (nook-number-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NOOK-WRITE-AST.
;;;
;;; This AST can be used to write a nook in a standard object.  It
;;; has three children, an AST that must have a standard object as
;;; its value, an AST that must have a fixnum as its value and that
;;; indicates a nook number (starting from 0), and an AST that
;;; generates the new value to store in the nook.  This AST generates
;;; no values.  An attempt to compile this AST in a context where a
;;; value is needed will result in an error being signaled.

(defclass nook-write-ast (ast no-value-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%nook-number-ast :initarg :nook-number-ast :reader nook-number-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(cleavir-io:define-save-info nook-write-ast
  (:object-ast object-ast)
  (:nook-number-ast nook-number-ast)
  (:value-ast value-ast))

(defmethod children ((ast nook-write-ast))
  (list (object-ast ast) (nook-number-ast ast) (value-ast ast)))
