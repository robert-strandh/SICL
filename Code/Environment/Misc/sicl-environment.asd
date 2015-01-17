(cl:in-package #:asdf-user)

;;;; This system defines a set of functions that can be thought of as
;;;; defining an intermediate layer of environment functions.  The
;;;; problem that it solves can be described as follows:
;;;;
;;;;   * Common Lisp functions such as CL:FDEFINITION either take an
;;;;     optional ENVIRONMENT object, or none at all.
;;;;
;;;;   * The functions in the SICL-GLOBAL-ENVIRONMENT module such as
;;;;     SICL-GLOBAL-ENVIRONMENT:FDEFINITION take a required global
;;;;     environment object.
;;;;
;;;;   * However, we are often given an environment object that can be
;;;;     local or global, typically in support code for a macro such
;;;;     as CL:ENSURE-GENERIC-FUNCTION, but if it is a local
;;;;     environment object, we need to access the global environment
;;;;     object at the end of the chain.
;;;;
;;;; This system solves the problem by introducing functions that can
;;;; take any environment object.  They then call the Cleavir function
;;;; for finding the global environment object at the end of the
;;;; chain, and finally they call the corresponding function in
;;;; SICL-GLOBAL-ENVIRONMENT.

(defsystem :sicl-environment
  :depends-on (:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "fdefinition-defun")))
