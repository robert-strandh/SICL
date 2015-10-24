(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant NIL.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push (make-constant-variable-entry nil nil)
	(constant-variables *global-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant T.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push (make-constant-variable-entry t t)
	(constant-variables *global-environment*)))
