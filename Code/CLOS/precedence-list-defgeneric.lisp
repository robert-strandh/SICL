(cl:in-package #:sicl-clos)

;;; This function is similar to CLASS-PRECEDENCE-LIST, except that
;;; CLASS-PRECEDENCE-LIST is specified to signal an error when the
;;; class is not finalized, which we accomplish by using a :BEFORE
;;; method.  However, during class finalization, we need to access the
;;; class precedence list in the two steps following its computation.
;;; Since at that point the class is not yet finalized, those two
;;; steps can not call CLASS-PRECEDENCE-LIST.  Our solution is to
;;; define an alternative reader for the same slot, named
;;; PRECEDENCE-LIST, and which does not signal an error if the class
;;; is not finalized.
(defgeneric precedence-list (class))
