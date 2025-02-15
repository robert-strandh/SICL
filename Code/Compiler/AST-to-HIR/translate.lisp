(cl:in-package #:sicl-ast-to-hir)

;;; When we translate a GO-WITH-VARIABLE-AST to an UNWIND-INSTRUCTION,
;;; the target instruction does not necessarily exist yet.  So in the
;;; SUCCESSORS slot of the UNWIND-INSTRUCTION, we store the contents
;;; of the INDEX slot of the AST and a vector created when the
;;; TAGBODY-WITH-VARIABLE-AST is translated.  We must then fix up
;;; those UNWIND-INSTRUCTIONs by replacing the SUCCESSORS slot by the
;;; instruction in the vector at the location of the index.  This
;;; variable holds the list of UNWIND-INSTRUCTIONs to fix up.

(defvar *unwind-instructions-to-fix-up*)

(defun fix-up-unwind-instructions ()
  (loop for unwind-instruction in *unwind-instructions-to-fix-up*
        for successors = (hir:successors unwind-instruction)
        for (index instruction-vector) = successors
        do (reinitialize-instance unwind-instruction
             :successors (list (aref instruction-vector index)))))

(defgeneric translate-ast (client ast))

(defun translate (client ast)
  (let* ((*registers* (make-hash-table :test #'eq))
         (*target-register*
           (make-instance 'hir:multiple-value-register))
         (*dynamic-environment-register*
           (make-instance 'hir:single-value-register))
         (*static-environment-register*
           (make-instance 'hir:single-value-register))
         (*values-count* :all)
         (*next-instruction*
           (make-instance 'hir:return-instruction
             :inputs (list *target-register*)))
         (*unwind-instructions-to-fix-up* '()))
    (translate-ast client ast)
    (fix-up-unwind-instructions)))
