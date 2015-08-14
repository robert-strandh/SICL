(cl:in-package #:cleavir-hir-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT-TO-IMMEDIATE.
;;;
;;; Before LOAD-TIME-VALUE-INPUTs are hoisted, we make a pass to see
;;; whether any of them should be converted to an IMMEDIATE-INPUT.
;;; Whether such a conversion should take place depends on the
;;; implementation.  For that reason, we call the generic function
;;; CONVERT-CONSTANT-TO-IMMEDIATE to do the conversion. 
;;;
;;; The function CONVERT-CONSTANT-TO-IMMEDIATE may return NIL, meaning
;;; that this constant does not have a representation as an immediate
;;; value, or it may return a possibly-negative integer which is
;;; taken to be the representation of the constant as an immediate
;;; machine word.  A default method is provided that always returns
;;; NIL.
;;;
;;; Client code should provide a method on this generic function that
;;; specialized on the SYSTEM parameter.

(defgeneric convert-constant-to-immediate (constant system))

(defmethod convert-constant-to-immediate (constant system)
  (declare (ignore constant system))
  nil)
