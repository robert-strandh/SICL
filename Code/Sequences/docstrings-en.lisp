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

(defparameter *sequence*
  (fmt "SEQUENCE is a proper sequence."))

(defparameter *index*
  (fmt "INDEX is a valid sequence index for SEQUENCE"))

(defparameter *item-sequence*
  (fmt "ITEM is any object, and SEQUENCE is a proper sequence."))

(defparameter *predicate-sequence*
  (fmt "PREDICATE is a designator for a function of one argument~@
        that returns a generalized boolean.~@
        SEQUENCE is a proper sequence."))

(defparameter *newitem-item-sequence*
  (fmt "NEWITEM is any object, ITEM is any object, and~@
        SEQUENCE is a proper sequence."))

(defparameter *newitem-predicate-sequence*
  (fmt "NEWITEM is any object.~@
        PREDICATE is a designator for a function of one argument~@
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

(defparameter *count*
  (fmt "COUNT is an integer or NIL.  When no value of COUNT is given, the~@
        default is NIL.  The value of COUNT limits the number of elements~@
        affected by the operation."))

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

(defparameter *error-not-valid-index*
  (fmt "An error of type TYPE-ERROR is signaled if INDEX is not a~@
        valid sequence index for SEQUENCE"))

(defparameter *maybe-error-bounding-indexes*
  (fmt "An error might be signaled if START and END are not valid bounding~@
	index designators for SEQUENCE, which means that 0 <= START <= L, and~@
	either END is NIL or 0 <= END <= L where L is the length of SEQUENCE."))

(defparameter *definitely-error-bounding-indexes*
  (fmt "An error is signaled if START and END are not valid bounding~@
	index designators for SEQUENCE, which means that 0 <= START <= L, and~@
	either END is NIL or 0 <= END <= L where L is the length of SEQUENCE."))

(defparameter *error-count*
  (fmt "The consequences are undefined if a value of COUNT other than NIL or~@
        an integer is given."))

(defparameter *find-description*
  (fmt "Searches the interval of SEQUENCE designated by START and END~@
        for an element of the sequence that satisfies the test.~@
	If such an element is found, then it is returned.~@
	Otherwise NIL is returned.~@
	~@
	If FROM-END is false, then the first element that satisfies the test is~@
	returned.  Otherwise the last element that satisfies the test is returned.~@
	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element.~@
        In particular, even if FROM-END is true, SEQUENCE may be search from the~@
        beginning to the end and the last element that satisfies the test returned."))

;;; Create documentation for a function.
(defun fundoc (name string)
  (setf (documentation name 'function) string)
  (setf (documentation (fdefinition name) 'function)
        (documentation name 'function)))

(fundoc 'copy-seq
	(fmt "Lambda list: (SEQUENCE)~@
              ~@
              Description:~@
              Returns a copy of SEQUENCE, i.e, a sequence that has the same length~@
              and the same elements of SEQUENCE in the same order.~@
              ~@
              If SEQUENCE is a vector, then the resulting sequence is a freshly~@
              allocated simple array of rank 1 with the same actual array element~@
              type as SEQUENCE.  Notice that it might not be a simple vector because~@
              simple vectors have an element type of t.  If SEQUENCE is a list, then~@
              the resulting sequence is a freshly-allocated list.~@
              ~@
              Arguments:~@
              SEQUENCE is a proper sequence.~@
              ~@
              Exceptional situations:~@
              ~a"
	     *error-not-proper-sequence*))

(fundoc 'elt
	(fmt "Lambda list: (SEQUENCE INDEX)~@
              ~@
              Description:~@
              Returns the element of SEQUENCE indicated by INDEX.~@
              ~@
              Arguments:~@
              ~a~%~a~@
              ~@
              Exceptional situations:~@
              ~a~%~a"
	     *sequence*
	     *index*
	     *error-not-proper-sequence*
	     *error-not-valid-index*))

;;; The CLHS entry for FILL is strange because it requires an error to be signaled
;;; if START is not a non-negative integer or if END is not either a non-negative
;;; integer or NIL.  However, it does not require an error to be signaled if START
;;; and END are otherwise invalid bounding index designators, for instance if 
;;; START > END.  In that case the default rule applies which says that the
;;; consequences are undefined if the restriction is not respected.  
;;;
;;; However, no sane implementation of FILL would fail to test for these restrictions
;;; and signal an error, so here we just assume that this is the case. 
;;;
;;; Question: What happens or should happen if ITEM is not the correct type as 
;;; an element of SEQUENCE?
(fundoc 'fill
	(fmt "Lambda list: (SEQUENCE ITEM &key START END)~@
              ~@
              Description:~@
              Destructively replaces the elements of SEQUENCE in the interval designated~@
              by START and END by ITEM.~@
              ~@
              Arguments:~@
              ~a~@
              ITEM is any object.~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~%~a"
	     *sequence* *bounding-indexes*
	     *error-not-proper-sequence*
	     *definitely-error-bounding-indexes*))

;;; The CLHS entry for MAKE-SEQUENCE says that the "consequences are
;;; unspecified" if INITIAL-ELEMENT is not an object that can be
;;; stored in the resulting sequence.  According to the error
;;; terminology in 1.4.2 of the CLHS this means that the consequences
;;; are unpredictable but harmless.  So the question here is: What do 
;;; typical implementations do in this case?  This docstring assumes
;;; that they signal en error of type TYPE-ERROR.
;;;
;;; The CLHS entry also says "If the result type is a subtype of
;;; VECTOR if the implementation can determine the element type
;;; specified for the result-type, the element type of the resulting
;;; array is the result of upgrading that element type; or, if the
;;; implementation can determine that the element type is unspecified
;;; (or *), the element type of the resulting array is t; otherwise,
;;; an error is signaled.".  But what does it mean to "determine" an
;;; element type?
;;; 
;;; FIXME: For now, I just reproduce what the CLHS says, but this is
;;; a temporary solution.
(fundoc 'make-sequence
	(fmt "Lambda list: (RESULT-TYPE SIZE &key INITIAL-ELEMENT)~@
              ~@
              Description:~@
              Returns a proper sequence.  The length of the sequence is SIZE,~@
              its type is RESULT-TYPE, and the elements are initialized to~@
              INITIAL-ELEMENT.~@
              ~@
              If RESULT-TYPE is a subtype of LIST, then the resulting sequence is~@
              a list.~@
              ~@
              If RESULT-TYPE is a subtype of VECTOR, then if the element type ~@
              specified for RESULT-TYPE can be determined, the element type of~@
              the resulting array is the result of upgrading that element type,~@
              or if it can be determined that the element type is unspecified~@
              then the element type of the resulting array is T.  Otherwise, an~@
              error is signaled.~@
              ~@
              Arguments:~@
              RESULT-TYPE is type specifier that specifies a sequence type.~@
              SIZE is a non-negative integer~@
              INITIAL-ELEMENT is any object.  The default is implementation dependent.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if INITIAL-ELEMENT is an~@
              object that can not be stored in the resulting sequence.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE is neither a~@
              recognizable subtype of LIST nor a recognizable subtype of VECTOR.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE specifies a~@
              size that is different from SIZE.~@
              An error is signaled if the element specified for RESULT-TYPE can not~@
              be determined."))

;;; The CLHS entry for LENGTH says that it "should be prepared to
;;; signal an error...  if sequence is not a proper sequence".  This
;;; phrase is meant for implementers of CL systems.  Here, we should
;;; try to describe what really happens.  We are assuming that an
;;; error is signaled for objects other than vectors and lists, and
;;; for dotted lists, so the only time an error is not signaled is when
;;; we have a circular list. 
(fundoc 'length
	(fmt "Lambda list: (SEQUENCE)~@
              ~@
              Description:~@
              Returns the length of SEQUENCE, i.e., the number of elements in it.~@
              LENGTH takes into account fill pointers of vectors, so that the the~@
              result is the value of the fill pointer in case SEQUENCE is a vector~@
              with a fill pointer.~@
              ~@
              Arguments:~@
              ~a~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is neither a VECTOR~@
              nor a LIST, and if SEQUENCE is a dotted list.  If SEQUENCE is a circular~@
              list, then the computation will not halt, and no value is returned.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For LENGTH this means in practice that an implementation might signal~@
              an error for circular lists, and that it might not signal an error for~@
              dotted lists."
	     *sequence*))

(fundoc 'subseq
	(fmt "Lambda list: (SEQUENCE START &optional END>)~@
              ~@
              Description:~@
              Creates a sequence containing the elements of SEQUENCE in the~@
              inteval designated by START and END, in the same order.~@
              ~@
              A fresh sequence is always allocated.  There is no top-level~@
              structure sharing between the resulting sequence and SEQUENCE.~@
              ~@
              If SEQUENCE is a vector, then the resulting sequence is a freshly~@
              allocated simple array of rank 1 with the same actual array element~@
              type as SEQUENCE.  Notice that it might not be a simple vector because~@
              simple vectors have an element type of t.  If SEQUENCE is a list, then~@
              the resulting sequence is a freshly-allocated list.~@
              ~@
              Arguments:~@
              ~a~@
              ~@
              START and END are bounding index designators.  They determine~@
              an interval with SEQUENCE that is considered.  This interval contains~@
              the indexes i such that START <= i < END.  The default for END is NIL,~@
              which means the end of the sequence.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a vector or~@
              a list, and if SEQUENCE is a dotted list, and the interval designated by~@
              START and END contains the last CONS cell of SEQUENCE.  No error is~@
              signaled if SEQUENCE is a circular list, or if SEQUENCE is a dotted list~@
              such that the interval designated by START and END does not contain the~@
              last CONS cell of SEQUENCE.~@
              ~@
              ~a"
	     *sequence*
	     *maybe-error-bounding-indexes*))

(fundoc 'remove-duplicates
	(fmt "Lambda list: (SEQUENCE &key KEY TEST TEST-NOT START END FROM-END)~@
              ~@
              Description:~@
              The elements in the interval of SEQUENCE designated by START and END~@
              are compared pairwise by applying the test.  If two elements satisfy~@
              the test, then the first one of the two will not be present in the~@
              resulting sequence.  If the two elements do not satisfy the test,~@
              then they will both be present in the resulting sequence.~@
              Elements in SEQUENCE that occur outside the interval designated by~@
              START and END will all be present in the resulting sequence.~@
              The relative order of any two elements in the resulting sequence will~@
              be the same as their relative order in SEQUENCE.~@
              ~@
              If SEQUENCE is a VECTOR, then the resulting sequence is also a VECTOR~@
              with the same actual element type as SEQUENCE.  If SEQUENCE is a LIST,~@
              then the resulting sequence is also a LIST.~@
              ~@
              The resulting sequence may share structure with SEQUENCE, and may be~@
              identical to SEQUENCE if no two elements in the interval of SEQUENCE~@
              designated by START and END satisfy the test.~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              "
	     *sequence* *key* *test-test-not* *bounding-indexes* *from-end*
	     *satisfy-a-two-argument-test*))

(fundoc 'delete-duplicates
	(fmt "Lambda list: (SEQUENCE &key KEY TEST TEST-NOT START END FROM-END)~@
              ~@
              Description:~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              "
	     *sequence* *key* *test-test-not* *bounding-indexes* *from-end*
	     *satisfy-a-two-argument-test*))

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
	     *maybe-error-bounding-indexes*))

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
	     *maybe-error-bounding-indexes*))

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
	     *maybe-error-bounding-indexes*))

(defparameter *count-description*
  (fmt "Counts the number of elements in the interval of SEQUENCE designated by~@
	START and END that satisfy the test, and returns the number of such elements.~@
	~@
	If FROM-END is false, then the elements of the designated interval will be~@
	tested from the beginning to the end of the sequence.  If FROM-END is true,~@
	then elements of the designated interval will be tested from the end to the~@
	beginning of the sequence.  This difference is important only when~@
	one of the functions implicated in the test has side effects."))

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
	     *maybe-error-bounding-indexes*))
            
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
	     *maybe-error-bounding-indexes*))

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
	     *maybe-error-bounding-indexes*))

(defparameter *position-description*
  (fmt "Searches the interval of SEQUENCE designated by START and END~@
        for an element of the sequence that satisfies the test.~@
	If such an element is found, then the position within SEQUENCE of~@
        the element found is returned.~@
	Otherwise NIL is returned.~@
	~@
	If FROM-END is false, then the position of the first element in the~@
        designated inteval of SEQUENCE that satisfies the test is returned.~@
	Otherwise the position of the last element in the designated inteval~@
        of SEQUENCE that satisfies the test is returned.  The position returned~@
        is relative to the beginning of the SEQUENCE and not of the interval.~@
	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element.~@
        In particular, even if FROM-END is true, SEQUENCE may be searched from~@
        the beginning to the end and the position of the last element that~@
        satisfies the test returned."))

(fundoc 'position
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
	     *position-description*
	     *item-sequence* *key* *test-test-not* *bounding-indexes* *from-end*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*))
            
(fundoc 'position-if
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
	     *position-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*))

(fundoc 'position-if-not
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
	     *position-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*))

(defparameter *remove-description*
  (fmt "Returns a sequence that is like SEQUENCE, except that the elements in~@
        the interval designated by START and END in SEQUENCE that satisfy the~@
        test are not present in the sequence returned. Otherwise the elements~@
        of the resulting sequence are the same, and in the same order, as those~@
        of SEQUENCE.~@
        ~@
        If SEQUENCE is a list, then the resulting sequence is a list.~@
        If SEQUENCE is a vector, then the resulting sequence is a vector with~@
        the same actual array element type as SEQUENCE.~@
        ~@
        The resulting sequence may be identical to SEQUENCE if no elements were~@
        removed, and if SEQUENCE is a list, then the resulting sequence may share~@
        structure with SEQUENCE.~@
        ~@
        When FROM-END is true, it is the last COUNT elements of the interval~@
        designated by START and END that are no longer present in the resulting~@
        sequence.  Otherwise, it is the first COUNT elements of the interval~@
        designated by START and END that are no longer present in the resulting~@
        sequence.  Thus, if COUNT is not given or NIL is given as the value of~@
        COUNT, it does not matter whether FROM-END is true or false.~@
        ~@
        When a non-NIL value of COUNT is supplied, at most COUNT elements in the~@
        interval designated by START and END are no longer present in the resulting~@
        sequence, as mentioned above.  Supplying a negative value for COUNT has the~@
        same effect as supplying the value 0 (zero).~@
        ~@
      	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element."))

(fundoc 'remove
	(fmt "Lambda list: (ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END COUNT)~@
              ~@
              Description:~@
              ~a~@
              ~@
	      Arguments:~@
	      ~a~%~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a~@
              ~a"
	     *remove-description*
	     *item-sequence* *key* *test-test-not* *bounding-indexes* *from-end* *count*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))
            
(fundoc 'remove-if
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *remove-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(fundoc 'remove-if-not
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *remove-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(defparameter *delete-description*
  (fmt "Returns a sequence that is like SEQUENCE, except that the elements in~@
        the interval designated by START and END in SEQUENCE that satisfy the~@
        test are not present in the sequence returned. Otherwise the elements~@
        of the resulting sequence are the same, and in the same order, as those~@
        of SEQUENCE. SEQUENCE might be modified as a result of this operation.~@
        ~@
        If SEQUENCE is a list, then the resulting sequence is a list.~@
        If SEQUENCE is a vector, then the resulting sequence is a vector with~@
        the same actual array element type as SEQUENCE.~@
        ~@
        SEQUENCE may be destroyed, and may be used to construct the resulting~@
        sequence.  The resulting sequence may or may not be identical to SEQUENCE.~@
        When SEQUENCE is a list, any CAR or CDR of its top-level list structure~@
        may be modified.~@
        When SEQUENCE is a vector, the length of SEQUENCE may be modified and its~@
        elements may be moved in order to produce the resulting sequence. 
        ~@
        When FROM-END is true, it is the last COUNT elements of the interval~@
        designated by START and END that are no longer present in the resulting~@
        sequence.  Otherwise, it is the first COUNT elements of the interval~@
        designated by START and END that are no longer present in the resulting~@
        sequence.  Thus, if COUNT is not given or NIL is given as the value of~@
        COUNT, it does not matter whether FROM-END is true or false.~@
        ~@
        When a non-NIL value of COUNT is supplied, at most COUNT elements in the~@
        interval designated by START and END are no longer present in the resulting~@
        sequence, as mentioned above.  Supplying a negative value for COUNT has the~@
        same effect as supplying the value 0 (zero).~@
        ~@
      	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element."))

(fundoc 'delete
	(fmt "Lambda list: (ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END COUNT)~@
              ~@
              Description:~@
              ~a~@
              ~@
	      Arguments:~@
	      ~a~%~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a~@
              ~a"
	     *delete-description*
	     *item-sequence* *key* *test-test-not* *bounding-indexes* *from-end* *count*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))
            
(fundoc 'delete-if
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *delete-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(fundoc 'delete-if-not
	(fmt "Lambda list: (PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *delete-description*
	     *predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(defparameter *substitute-description*
  (fmt "Returns a copy of SEQUENCE in which every element in the interval designated~@
        START and END that satisfies the test has been replaced by NEWITEM.~@
         ~@
        If SEQUENCE is a list, then the resulting sequence is a list.~@
        If SEQUENCE is a vector, then the resulting sequence is a vector with~@
        the same actual array element type as SEQUENCE.~@
        ~@
        The resulting sequence may be identical to SEQUENCE if no elements were~@
        replaced, and if SEQUENCE is a list, then the resulting sequence may share~@
        structure with SEQUENCE.~@
        ~@
        When FROM-END is true, it is the last COUNT elements of the interval~@
        designated by START and END that are replaced in the resulting~@
        sequence.  Otherwise, it is the first COUNT elements of the interval~@
        designated by START and END that are replaced in the resulting~@
        sequence.  Thus, if COUNT is not given or NIL is given as the value of~@
        COUNT, it does not matter whether FROM-END is true or false.~@
        ~@
        When a non-NIL value of COUNT is supplied, at most COUNT elements in the~@
        interval designated by START and END are replaced in the resulting~@
        sequence, as mentioned above.  Supplying a negative value for COUNT has the~@
        same effect as supplying the value 0 (zero).~@
        ~@
      	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element."))
       
(fundoc 'substitute
	(fmt "Lambda list: (NEWITEM ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END COUNT)~@
              ~@
              Description:~@
              ~a~@
              ~@
	      Arguments:~@
	      ~a~%~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a~@
              ~a"
	     *substitute-description*
	     *newitem-item-sequence* *key* *test-test-not* *bounding-indexes* *from-end* *count*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))
            
(fundoc 'substitute-if
	(fmt "Lambda list: (NEWITEM PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *substitute-description*
	     *newitem-predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(fundoc 'substitute-if-not
	(fmt "Lambda list: (NEWITEM PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *substitute-description*
	     *newitem-predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(defparameter *nsubstitute-description*
  (fmt "Returns SEQUENCE in which every element in the interval designated~@
        START and END that satisfies the test has been replaced by NEWITEM.~@
         ~@
        When FROM-END is true, it is the last COUNT elements of the interval~@
        designated by START and END that are replaced in the resulting~@
        sequence.  Otherwise, it is the first COUNT elements of the interval~@
        designated by START and END that are replaced in the resulting~@
        sequence.  Thus, if COUNT is not given or NIL is given as the value of~@
        COUNT, it does not matter whether FROM-END is true or false.~@
        ~@
        When a non-NIL value of COUNT is supplied, at most COUNT elements in the~@
        interval designated by START and END are replaced in the resulting~@
        sequence, as mentioned above.  Supplying a negative value for COUNT has the~@
        same effect as supplying the value 0 (zero).~@
        ~@
      	There is no requirement that the test will be applied in any particular~@
        order, nor that it will be applied at most once to a particular element."))
       
(fundoc 'nsubstitute
	(fmt "Lambda list: (NEWITEM ITEM SEQUENCE &key KEY TEST TEST-NOT START END FROM-END COUNT)~@
              ~@
              Description:~@
              ~a~@
              ~@
	      Arguments:~@
	      ~a~%~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              ~a~@
              ~a~@
              ~a"
	     *nsubstitute-description*
	     *newitem-item-sequence* *key* *test-test-not* *bounding-indexes* *from-end* *count*
	     *satisfy-a-two-argument-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))
            
(fundoc 'nsubstitute-if
	(fmt "Lambda list: (NEWITEM PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *nsubstitute-description*
	     *newitem-predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-positive-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))

(fundoc 'nsubstitute-if-not
	(fmt "Lambda list: (NEWITEM PREDICATE SEQUENCE &key KEY START END FROM-END COUNT)~@
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
              ~a~@
              ~a"
	     *nsubstitute-description*
	     *newitem-predicate-sequence* *key* *bounding-indexes* *from-end* *count*
	     *satisfy-a-one-argument-negative-test*
	     *error-not-proper-sequence*
	     *maybe-error-bounding-indexes*
	     *error-count*))
