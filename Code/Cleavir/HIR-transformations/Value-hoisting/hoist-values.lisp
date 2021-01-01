(cl:in-package #:cleavir-value-hoisting)

;;; The purpose of this transformation is to replace all non-immediate
;;; constants by outputs of some sequences of instructions, and to arrange
;;; that these instructions are executed at load time and in the correct
;;; order.
;;;
;;; Some parts of this transformation are necessarily client specific,
;;; e.g., it is not possible to reconstruct a symbol at load time by means
;;; of (funcall 'intern name package), because intern is itself a symbol,
;;; leading to infinite recursion.  The following client specific methods
;;; are mandatory for this transformation to succeed:
;;;
;;; - A default method on HIR-FROM-FORM, that converts a form to the
;;;   corresponding HIR.
;;;
;;; - Methods on MAKE-LOAD-FORM-USING-CLIENT and on SIMPLIFY-DATUM to
;;;   circumvent circular definitions, especially for fixnums, characters,
;;;   symbols and strings.
;;;
;;; A client may also provide further methods for SIMPLIFY-DATUM and
;;; SIMILARITY-KEYS.

(defvar *eql-table*)

(defvar *equal-table*)

(defvar *equalp-table*)

(defvar *environment*)

(defun hoist-values (client hir environment)
  (let ((*eql-table* (make-hash-table :test #'eql))
        (*equal-table* (make-hash-table :test #'equal))
        (*equalp-table* (make-hash-table :test #'equalp))
        (*environment* environment))
    (scan-hir client hir)
    (hoist-toplevel-hir client hir)))
