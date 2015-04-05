(cl:in-package #:sicl-evaluation-and-compilation)

(defun macro-function (symbol &optional environment)
  (cleavir-env:macro-function
   symbol
   (or environment
       (load-time-value (sicl-genv:global-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF MACRO-FUNCTION).
;;;
;;; The HyperSpec says that the consequences are undefined if a
;;; non-nil environment is given.  We define those consequences to be
;;; that the environment is considered to be a first-class global
;;; environment.

(defun (setf macro-function) (new-function symbol &optional environment)
  (declare (type symbol symbol)
	   (type function new-function))
  (when (null environment)
    (setf environment
	  (load-time-value (sicl-genv:global-environment))))
  (setf (sicl-genv:macro-function symbol environment)
	new-function))
