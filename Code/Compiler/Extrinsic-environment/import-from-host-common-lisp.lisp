(cl:in-package #:sicl-extrinsic-environment)

(defun import-special-operators (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	when (special-operator-p symbol)
	  do (setf (sicl-env:special-operator symbol environment) t)))

(defun import-nil-and-t (environment)
  (setf (sicl-env:constant-variable t environment) t)
  (setf (sicl-env:constant-variable nil environment) nil))

;;; Enter every Common Lisp class into the environment.
(defun import-classes (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	for class = (find-class symbol nil)
	unless (null class)
	  do (setf (sicl-env:find-class symbol environment)
		   class)))

(defun import-variables (environment)
  (let ((variables '(*standard-input* *standard-output* *error-output*
		     *terminal-io* *trace-output* *query-io*
		     *print-base* *read-base* *package*)))
    (loop for symbol in variables
	  do (setf (sicl-env:special-variable symbol environment t)
		   (cl:symbol-value symbol)))))

;;; Rather than explicitly enumerating the functions to import, we
;;; just import them all initially.  Later, we redefine the ones for
;;; which the host definition will not be appropriate.
(defun import-functions (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	when (and (fboundp symbol)
		  (null (macro-function symbol))
		  (not (special-operator-p symbol)))
	  do (setf (sicl-env:fdefinition symbol environment) t)
	when (fboundp `(setf ,symbol))
	  do (setf (sicl-env:fdefinition `(setf ,symbol) environment) t)))

(defun import-from-host-common-lisp (environment)
  (import-special-operators environment)
  (import-nil-and-t environment)
  (import-classes environment)
  (import-variables environment))
  
