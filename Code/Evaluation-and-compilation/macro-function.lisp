(cl:in-package #:sicl-evaluation-and-compilation)

(defun macro-function
    (symbol &optional (environment (sicl-genv:global-environment)))
  (trucler:macro-function symbol environment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF MACRO-FUNCTION).
;;;
;;; The HyperSpec says that the consequences are undefined if a
;;; non-nil environment is given.  We define those consequences to be
;;; that the environment is considered to be a first-class global
;;; environment.

(defun (setf macro-function)
    (new-function symbol &optional (environment (sicl-genv:global-environment)))
  (setf (sicl-genv:macro-function symbol environment)
	new-function))
