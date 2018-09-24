(cl:in-package #:sicl-clos)

;;;; The AMOP specifies that the three generic functions
;;;; CLASS-PRECEDENCE-LIST, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS
;;;; should signal an error if they are called on a class that has not
;;;; yet been finalized.
;;;;
;;;; There are several ways of accomplishing that action.  One
;;;; possibility would be to have these functions NOT be defined as
;;;; slot readers, and to use some other way of accessing the slot,
;;;; either by using a different reader or by using SLOT-VALUE.  The
;;;; disadvantage of this solution is that either another generic
;;;; function has to be introduced, or we have to use SLOT-VALUE which
;;;; is not necessarily very fast.  
;;;;
;;;; Here, we use a different way of accomplishing that action.  We
;;;; define those three functions as slot readers, but we introduce
;;;; :BEFORE methods that check that the class has been finalized, and
;;;; signal an error if this is not the case.
;;;;
;;;; We define these :BEFORE methods separately, because during
;;;; bootstrapping, there are times when we do not want to call
;;;; CLASS-FINALIZED-P.  And during bootstrapping, we do not really
;;;; need to check whether the class has been finalized, because we
;;;; make sure it has been before calling any of these generic
;;;; functions.
;;;;
;;;; Note: The AMOP specifies the existence of a primary method on
;;;; each of these three generic functions, specialized to
;;;; FORWARD-REFERENCED-CLASS, and each such primary method is
;;;; specified to signal an error.  We comply with the specification
;;;; and supply such primary methods, but they will never be called in
;;;; the presence of the :BEFORE methods in this file.  The reason is
;;;; that the AMOP also specifies the existence of a primary method on
;;;; CLASS-FINALIZED-P specialized to FORWARD-REFERENCED-CLASS that
;;;; should return false.  So when one of the three generic functions
;;;; in this file is called with a forward referenced class, the
;;;; :BEFORE method will detect that the class is not finalized and
;;;; signal an error, so that the primary method is never called. 

(defmethod class-precedence-list :before (class)
  (unless (class-finalized-p class)
    (error 'attempt-to-access-precedence-list-of-unfinalized-class
           :offending-class class)))

(defmethod class-default-initargs :before (class)
  (unless (class-finalized-p class)
    (error 'attempt-to-access-the-default-initargs-of-unfinalized-class
           :offending-class class)))

(defmethod class-slots :before (class)
  (unless (class-finalized-p class)
    (error 'attempt-to-access-the-effective-slots-of-unfinalized-class
           :offending-class class)))
