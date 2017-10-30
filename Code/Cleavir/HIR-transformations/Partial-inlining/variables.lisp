(cl:in-package #:cleavir-partial-inlining)

;;; This variable contains the original ENTER-INSTRUCTION of the
;;; function being inlined.  It is used to determine whether a
;;; reference to a lexical location in the function being inlined is
;;; owned by that function, or whether, on the contrary, it is a
;;; reference to a lexical location in some ancestor function.

(defvar *original-enter-instruction*)
