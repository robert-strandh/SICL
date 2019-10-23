(in-package #:sicl-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fmt (&rest args)
    (apply #'format nil args)))

;;; Create documentation for a function.
(defun fundoc (name string)
  (setf (documentation name 'function) string)
  (setf (documentation (fdefinition name) 'function)
        (documentation name 'function)))

;;; FIXME: Add more text.
(fundoc 'export
        (fmt "Lambda list: (symbols &optional package)~@
              SYMBOLS is a designator for a list of symbols.~@
              PACKAGE is a package designnator.~@
              The default value for PACKAGE is the current package.~@
              This function makes the designated symbols external~@
              in PACKAGE.~@
              "))
