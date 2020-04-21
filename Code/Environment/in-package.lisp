;;;; The purpose of this file is to create the macro IN-PACKAGE.

;;; When this file is compiled by the cross compiler, we already have
;;; a temporary definition of IN-PACKAGE in place, so we can use it.
(cl:in-package #:sicl-global-environment)

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package*
	   (find-package ',string-designator
			 (global-environment)))))
