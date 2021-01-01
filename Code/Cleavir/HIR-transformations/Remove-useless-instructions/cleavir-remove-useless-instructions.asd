(cl:in-package #:asdf-user)

;;;; The purpose of this system is to remove useless instructions from
;;;; a HIR program.  An instruction is useless if a call to
;;;; INSTRUCTION-MAY-BE-REMOVED-P with the instruction as an argument
;;;; returns true.
;;;;
;;;; The default method on INSTRUCTION-MAY-BE-REMOVED-P returns true
;;;; if the instruction has a single successor and none of the
;;;; locations that it writes is ever used.
;;;;
;;;; Certain instructions can not be removed because they may have
;;;; side effects, or for some other reason.  The ENTER-INSTRUCTION
;;;; can never be removed.  The UNWIND-INSTRUCTION has a side effect,
;;;; namely to unwind the stack.  Similarly, the RETURN-INSTRUCTION
;;;; has the side effect of terminating the current function
;;;; invocation.
;;;;
;;;; The FUNCALL-INSTRUCTION is a special case.  A FUNCALL-INSTRUCTION
;;;; can be removed if the function that it calls has no side effects.
;;;; However, currently, this information is not available to us.  For
;;;; that reason, the method on INSTRUCTION-MAY-BE-REMOVED-P
;;;; specialized to FUNCALL-INSTRUCTION currently returns NIL no
;;;; matter what function is called.

(defsystem #:cleavir-remove-useless-instructions
  :depends-on (#:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "remove-useless-instructions")))

;;  LocalWords:  FUNCALL
