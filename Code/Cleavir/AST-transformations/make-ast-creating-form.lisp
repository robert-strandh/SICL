(cl:in-package #:cleavir-ast-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making a form that creates an AST.
;;;
;;; When an AST needs to be saved so that it can later be inlined it
;;; is often necessary to make sure the AST is saved when some FASL
;;; file is loaded.  It must therefore be possible to create an AST at
;;; load time.  We do this by creating a form (to be used with
;;; LOAD-TIME-VALUE) from an AST that creates an AST isomorphic to the
;;; original one.

(defun create-variable-dictionary (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (setf (gethash node dictionary) (gensym)))
     ast)
    dictionary))
