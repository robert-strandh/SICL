(cl:in-package #:cleavir-partial-inlining)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WORKLIST-ITEM.
;;;
;;; Instances of this class determine a unit of work to be done by the
;;; function INLINE-ONE-INSTRUCTION.  We manage these items in a
;;; worklist.  The exact organization of the worklist is not
;;; important.

(defclass worklist-item ()
  ((%enclose :initarg :enclose-instruction :reader enclose-instruction)
   (%call :initarg :call-instruction :reader call-instruction)
   (%enter :initarg :enter-instruction :reader enter-instruction)
   (%mapping :initarg :mapping :reader mapping)))
