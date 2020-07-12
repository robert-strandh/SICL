(cl:in-package #:portable-condition-system)

;;; Definition of RESTART as a structure class.

(defstruct restart
  "A restart structure, implementing the ANSI CL system class RESTART."
  (name (error "NAME required."))
  (function (error "FUNCTION required."))
  (report-function nil)
  (interactive-function nil)
  (test-function (constantly t))
  (associated-conditions '()))
