(cl:in-package #:cleavir-cst)

(defclass cst ()
  (;; This slot contains the Common Lisp expression that is
   ;; represented by the CST.  It is guaranteed that there is sharing
   ;; between the expression of a CST and the expression of each child
   ;; of the CST.
   (%expression :initarg :expression :reader expression)
   ;; This slot contains the source location of the expression
   ;; represented by the CST, or NIL if such source location is not
   ;; available.  The nature of the contents of this slot is not
   ;; determined by Cleavir, but by client code.  It is just used to
   ;; pass on information from the CST to the AST, and then to HIR.
   (%location :initarg :location :reader location)
   ;; This slot contains a list of child CSTs.  If this slot contains
   ;; the empty list, then the expression represented by this CST is
   ;; an atom.  If the expression represented by this CST is a dotted
   ;; list, then this slot contains a dotted list where the CDR of the
   ;; last CONS cell contains a CST.  If the expression represented by
   ;; this CST is a circular list, then this slot also contains a
   ;; circular list.
   (%children :initarg :children :reader children)))
