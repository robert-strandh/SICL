(cl:in-package #:sicl-clos)

;;;; This file contains an implementation of generic function dispatch
;;;; that is very basic, and as a consequence, very slow.
;;;;
;;;; We bootstrap SICL from a small image.  It is small in that it
;;;; contains as little functionality as possible but it is big enough
;;;; that a complete system can be obtained by loading additional FASL
;;;; files.  SICL does not start with a pre-CLOS Lisp system and then
;;;; adds something similar to Portable Common Loops later.  It
;;;; contains CLOS from the beginning.
;;;;
;;;; The reason for the existence of this basic implementation of
;;;; generic function dispatch is that, any of the more fancy
;;;; implementations of it requires the native compiler to be present.
;;;; But an initial image with the compiler present is not what we
;;;; consider `small'.
;;;;
;;;; This implementation is useful for other purposes as well.  It can
;;;; be used as a basic implementation for testing more fancy
;;;; implementations.  It could also be used for deployment of
;;;; applications for which it is undesireable to have the compiler present. 
;;;;
;;;; Our implementation is similar to that of Chapter 1 of the Art of
;;;; the Metaobject Protocol.  It differs in many places, because we
;;;; want to avoid using fancy functionality that might not exist in
;;;; the early stages of bootstrapping, or functionality that might
;;;; introduce metastability problems. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary function INSTANCE-OF.
;;;
;;; Determine whether an object O is an instance of some class C or
;;; some subclass of C.  We do this by checking whether the class of O
;;; (as reported by CLASS-OF) is a (not necessarily strict) subclass
;;; of C.

(defun instance-of (object class)
  (subclassp (class-of object) class))
