(cl:in-package #:common-lisp-user)

(defpackage #:sicl-primop
  (:use)
  (:export
   ;; This primop has no parameters.  It returns the current dynamic
   ;; environment.  We use it to write certain functions, such as the
   ;; ones that augment the current dynamic environment with some
   ;; entry.
   #:dynamic-environment
   ;; The following two primops return the stack pointer and the frame
   ;; pointer of the caller of the function in which these primops are
   ;; called.  We use them to write the function that augments the
   ;; dynamic environment with a BLOCK/TAGBDODY entry.  That entry
   ;; contains the stack pointer and the frame pointer that must be
   ;; restored when the stack is unwound.
   #:caller-stack-pointer
   #:caller-frame-pointer
   ;; This primop has two parameters.  It does not return any values.
   ;; The effect is that the parameters are used in order to install a
   ;; new stack pointer and a new frame pointer.
   #:establish-stack-frame
   ;; This primop has at least one parameter.  The first parameter is
   ;; a form that computes a dynamic environment.  The remaining
   ;; parameters are forms that are executed in the dynamic
   ;; environment computed by the first form.  The remaining forms are
   ;; an implicit PROGN.
   #:with-dynamic-environment))
