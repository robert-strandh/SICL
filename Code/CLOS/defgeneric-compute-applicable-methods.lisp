(cl:in-package #:sicl-clos)

;;; Given a generic function and a list of arguments to that generic
;;; function, return a list of the methods that are applicable to
;;; those arguments.  The methods in the list are ordered from most
;;; specific to least specific.  If there are no applicable methods,
;;; then the empty list is returned.
;;;
;;; The list of arguments may contain more arguments than there are
;;; required parameters of the generic functions, so that the caller
;;; can simply pass the entire list of arguments without truncating it
;;; to the number of required arguments.  This function simply ignores
;;; arguments beyond the last required arguments.

