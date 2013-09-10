;;;; The purpose of this file is to create the macro IN-PACKAGE.

;;; When this file is compiled by the cross compiler, we already have
;;; a temporary definition of IN-PACKAGE in place, so we can use it.
(in-package #:sicl-compiler-environment)

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (in-package-function ',string-designator)))
