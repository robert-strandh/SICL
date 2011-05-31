(in-package #:sicl-sequences)

;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Robert Strandh (strandh@labri.fr)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the sequences module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file sequences.text for a description of the module.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fmt (&rest args)
    (apply #'format nil args)))

(defparameter *item-sequence*
  (fmt "ITEM is any object, and SEQUENCE is a proper sequence."))

(defparameter *predicate-sequence*
  (fmt "PREDICATE is a designator for a function of one argument~@
        that returns a generalized boolean.~@
        SEQUENCE is a proper sequence."))

(defparameter *key*
  (fmt "KEY is a designator for a function of one argument which is~@
        applied the elements of SEQUENCE before the test is applied,~@
        or KEY could be NIL which means IDENTITY."))

(defparameter *test-test-not*
  (fmt "TEST and TEST-NOT are designators for functions of two arguments~@
        that return a generalized boolean indicating whether the test passed.~@
        The default if neither TEST nor TEST-NOT is given is a TEST of EQL."))

(defparameter *bounding-indexes*
  (fmt "START and END are bounding index designators.  They determine~@
        an interval with SEQUENCE that is considered.  This interval contains~@
        the indexes i such that START <= i < END.  The default for START is 0,~@
        and the default for END is NIL, which means the end of the sequence."))

(defparameter *satisfy-a-two-argument-test*
  (fmt "To determine whether an element satisfies the test,  The KEY function~@
        is first applied to the element.  The result is then used in the test.~@
	Then, if TEST is given, TEST is applied to ITEM and the result of~@
	applying the KEY function.  If TEST returns true, then the element~@
	satisfies the test.  Otherwise the element does not satisfy the test.~@
	If instead TEST-NOT is given, then TEST-NOT is is applied to ITEM and~@
	the result of applying the KEY function. If TEST-NOT returns false, then~@
	the element satisfies the test.  Otherwise the element does not~@
	satisfy the test."))

(defparameter *satisfy-a-one-argument-positive-test*
  (fmt "To determine whether an element satisfies the test, the KEY function is~@
        first applied to the element.  Then PREDICATE is applied to the result.~@
        If PREDICATE returns true, then the element satisfies the test.~@
        Otherwise, the element does not satisfy the test."))

(defparameter *satisfy-a-one-argument-negative-test*
  (fmt "To determine whether an element satisfies the test, the KEY function is~@
        first applied to the element.  Then PREDICATE is applied to the result.~@
        If PREDICATE returns false, then the element satisfies the test.~@
        Otherwise, the element does not satisfy the test."))

(defparameter *from-end*
  (fmt "FROM-END is a generalized boolean.  The default-value for FROM-END is false."))

(defparameter *error-not-proper-sequence*
  (fmt "An error of type TYPE-ERROR might be signaled if SEQUENCE is not a~@
        proper sequence."))

(defparameter *error-bounding-indexes*
  (fmt "An error might be signaled if START and END are not valid bounding~@
	index designators for SEQUENCE, which means that 0 <= START <= L, and~@
	either END is NIL or 0 <= END <= L where L is the length of SEQUENCE."))

(defparameter *find-description*
  (fmt "Searches the interval of SEQUENCE designated by START and END~@
        for an element of the sequence that satisfies the test.~@
	If such an element is found, then it is returned.~@
	Otherwise NIL is returned.~@
	~@
	If FROM-END is false, then the first element that satisfies the test is~@
	returned.  Otherwise the last element that satisfies the test is returned.~@
	There is no requirement, however that the KEY and the TEST or TEST-NOT~@
	functions will be applied in any particular order.  In particular, even if~@
	FROM-END is true, SEQUENCE may be search from the beginning to the end~@
	and the last element that satisfies the test returned."))

;;; Create documentation for a function.
(defun fundoc (name string)
  (setf (documentation name 'function) string)
  (setf (documentation (fdefinition name) 'function)
        (documentation name 'function)))

(fundoc 'find
	(fmt "Lambda list: (ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *find-description*
	     *item-sequence* *key* *test-test-not* *bounding-indexes* *from-end*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))

(fundoc 'find-if
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *find-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))

(fundoc 'find-if-not
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *find-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))

(defparameter *count-description*
  (fmt "Counts the number of elements in the interval of SEQUENCE designated by~@
	START and END that satisfy the test, and returns the number of such elements.~@
	~@
	If FROM-END is false, then the elements of the designated interval will be~@
	tested from the beginning to the end of the sequence.  If FROM-END is true,~@
	then elements of the designated interval will be tested from the end to the~@
	beginning of the sequence.  This difference is important only when~@
	KEY, TEST, or TEST-NOT has side effects."))

(fundoc 'count
	(fmt "Lambda list: (ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
	      Arguments:~@
	      ~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *count-description*
	     *item-sequence* *key* *test-test-not* *bounding-indexes* *from-end*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))
            
(fundoc 'count-if
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *count-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))

(fundoc 'count-if-not
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END)~@
              ~@
              Description:~@
              ~a~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a"
	     *count-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *error-bounding-indexes*))
