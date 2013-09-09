;;;; The purpose of this file is to create the macro IN-PACKAGE.

;;; Since IN-PACKAGE does not exist when this file is compiled, we
;;; must do what IN-PACKAGE does manually.  In this case, we set the
;;; *PACKAGE* variable to the package SICL-ENV.
(eval-when (:compile-toplevel)
  (setq *package* (find-package '#:sicl-env)))

;;; The other thing that does not yet exist when this file is compiled
;;; is QUASIQUOTE macro that is required when the BACKQUOTE reader
;;; macro is used.  For that reason, we can't use BACKQUOTE either, so
;;; we have to do it manually. 
;;;
;;; What we really want is:
;;; (defmacro in-package (string-designator)
;;;  `(eval-when (:compile-toplevel :load-toplevel :execute)
;;;     (in-package-function ',string-designator)))

(defmacro in-package (string-designator)
  (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
	(list 'in-package-function
	      (list 'quote string-designator))))

