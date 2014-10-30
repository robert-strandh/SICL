;;;; This file is the first one to be compiled by the cross compiler.
;;;; When it is compiled, the global environment is empty.  In
;;;; particular, it has no macros in it.  The purpose of the code in
;;;; this file is to create the first macro DEFMACRO.  But this must
;;;; be done "manually".

;;; Since IN-PACKAGE is a macro, it does not exist when this file is
;;; compiled, so we must do what IN-PACKAGE does manually.  In this
;;; case, we set the *PACKAGE* variable to the package SICL-ENV.
(eval-when (:compile-toplevel)
  (setq *package* (find-package '#:sicl-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFMACRO.
;;;
;;; We can obviously not define DEFMACRO using DEFMACRO.  But we have
;;; all the ingredients to define DEFMACRO "manually".

(eval-when (:compile-toplevel)
  (funcall
   #'(setf macro-function)
   (compile nil
	    (cleavir-code-utilities:parse-macro
	     'defmacro
	     '(name lambda-list &body body)
	     '(`(eval-when (:compile-toplevel :load-toplevel :execute)
		  (funcall #'(setf macro-function)
			   (function ,(cleavir-code-utilities:parse-macro
				       name
				       lambda-list
				       body))
			   ',name)))))
   'defmacro))


