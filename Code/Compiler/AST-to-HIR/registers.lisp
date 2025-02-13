(cl:in-package #:sicl-ast-to-hir)

;;; During translation, this variable contains a hash table mapping
;;; VARIABLE-DEFINITION-ASTs to registers.
(defvar *registers*)

(defun find-register (variable-definition-ast)
  (multiple-value-bind (register present-p)
      (gethash variable-definition-ast *registers*)
    (assert present-p)
    register))

(defun (setf find-register) (register variable-definition-ast)
  (setf (gethash variable-definition-ast *registers*) register))

        
