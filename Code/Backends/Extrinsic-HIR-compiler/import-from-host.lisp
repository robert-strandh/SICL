(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; Make sure the functionality of the host COMMON-LISP package is
;;; available in the environment under the same names, but in the
;;; package HOST-COMMON-LISP.
(loop for symbol being each external-symbol in '#:common-lisp
      when (and (fboundp symbol)
		(not (special-operator-p symbol))
		(null (macro-function symbol)))
	do (setf (sicl-env:fdefinition
		  symbol
		  *environment*)
		 (fdefinition symbol))
	   (setf (sicl-env:fdefinition
		  (find-symbol (symbol-name symbol)
			       '#:host-common-lisp)
		  *environment*)
		 (fdefinition symbol))
      when (fboundp (list 'setf symbol))
	do (setf (sicl-env:fdefinition
		  (list 'setf symbol)
		  *environment*)
		 (fdefinition (list 'setf symbol)))
	   (setf (sicl-env:fdefinition
		  (list 'setf
			(find-symbol (symbol-name symbol)
				     '#:host-common-lisp))
		  *environment*)
		 (fdefinition (list 'setf symbol))))

;;; Enter every Common Lisp special operator into the environment.
;;; We can take them from the host environment.
(loop for symbol being each external-symbol in '#:common-lisp
      when (special-operator-p symbol)
	do (setf (sicl-env:special-operator symbol *environment*) t))

;;; Enter every Common Lisp class into the environment.
(loop for symbol being each external-symbol in '#:common-lisp
      for class = (find-class symbol nil)
      unless (null class)
	do (setf (sicl-env:find-class symbol *environment*)
		 class))

;;; Define NIL and T as constant variables.
(setf (sicl-env:constant-variable t *environment*) t)
(setf (sicl-env:constant-variable nil *environment*) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *imported-variables*
    '(*standard-input* *standard-output* *error-output*
      *terminal-io* *trace-output* *query-io*
      *print-base* *read-base*)))

(loop for symbol in *imported-variables*
      do (setf (sicl-env:special-variable 'cl:*package* *environment* t)
	       (cl:symbol-value symbol)))

;;; Set the variable CL:*PACKAGE* in the environment.
(setf (sicl-env:special-variable 'cl:*package* *environment* t)
      (find-package '#:common-lisp-user))

;;; Add every global environment function into the environment.
(loop for symbol being each external-symbol in '#:sicl-env
      when (fboundp symbol)
	do (setf (sicl-env:fdefinition symbol *environment*)
		 (fdefinition symbol))
      when (fboundp `(setf ,symbol))
	do (setf (sicl-env:fdefinition `(setf ,symbol) *environment*)
		 (fdefinition `(setf ,symbol))))

;;; Add every intermediate environment function into the environment.
(loop for symbol being each external-symbol in '#:sicl-environment
      when (fboundp symbol)
	do (setf (sicl-env:fdefinition symbol *environment*)
		 (fdefinition symbol))
      when (fboundp `(setf ,symbol))
	do (setf (sicl-env:fdefinition `(setf ,symbol) *environment*)
		 (fdefinition `(setf ,symbol))))

;;; Import the SICL reader as CL:READ.
(setf (sicl-env:fdefinition 'read *environment*)
      (fdefinition 'sicl-reader:read))
