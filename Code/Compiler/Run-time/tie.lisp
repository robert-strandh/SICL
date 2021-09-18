(cl:in-package #:sicl-run-time)

;;; When a code object is tied to a particular environment, this
;;; variable holds the code vector containing the native instructions.
(defvar *code-vector*)

;;; When a code object is tied to a particular environment, this
;;; variable holds the vector of literals that the garbage collector
;;; will traverse in order to keep those literals live.
(defvar *literals-vector*)

;;; After the code generated from a LOAD-TIME-VALUE form is executed,
;;; this function is called with the object resulting from the
;;; computation, an index into the code vector where the object should
;;; be stored as an immediate value in the instruction stream, and an
;;; index into the literals vector where the object should be stored.
(defun resolve-load-time-value
    (literal code-vector-index literals-vector-index)
  (declare (ignore literal code-vector-index literals-vector-index))
  ;; FIXME: do the action.
  nil)
