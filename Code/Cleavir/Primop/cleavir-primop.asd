;;;; Primitive operations (or primops for short) are similar to Common
;;;; Lisp special operators in that the compiler handles them
;;;; specially.  They are different from Common Lisp special operators
;;;; in that a primop does not necessarily have an evaluation rule
;;;; that is different from that of a function call.
;;;;
;;;; Generally speaking, primops should not be used directly in
;;;; application code.  Instead, they are used in system code for
;;;; implementing certain basic Common Lisp function.  So, for
;;;; example, the CONSP primop would typically be used only in the
;;;; code for the Common Lisp function CONSP.  That function would
;;;; then be inlined by the compiler, so that the resulting AST and
;;;; ultimately the resulting HIR instruction that the CONSP primop
;;;; translates to will be present also in application code.
;;;;
;;;; Frequently, the need for a primop comes from arises because some
;;;; HIR instruction is needed.  Take, for example, the HIR
;;;; instruction named EQ-INSTRUCTION for comparing two pointer values
;;;; for equality.  The existence of that instruction requires an AST
;;;; doing the same thing, and it is called EQ-AST.  When compiled to
;;;; HIR, the EQ-AST generates the an EQ-INSTRUCTION.  Finally, in
;;;; order to produce the source for the Common Lisp function EQ, it
;;;; must be possible to produce the EQ-AST from some Common Lisp
;;;; code, which is why the EQ primop is needed.

(cl:in-package #:asdf-user)

(defsystem :cleavir-primop
  :components
  ((:file "packages")))
