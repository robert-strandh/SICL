(cl:in-package #:sicl-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPILATION-UNIT.

(defclass compilation-unit ()
  (;; The linkage vector is a simple vector.  It is common to all
   ;; functions of the compilation unit.  The first element of the
   ;; linkage vector is a back pointer to the compilation unit.
   (%linkage-vector :initarg :linkage-vector)
   ;; This slot contains a vector with element type (unsigned-byte 8),
   ;; or perhaps something else for backends where each instruction is
   ;; an entire word.
   (%code :initarg :code)))
