(cl:in-package #:sicl-sequence)

;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
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
        applied to the elements of SEQUENCE before the test is applied,~@
        or KEY could be NIL which means IDENTITY."))

(defparameter *test-test-not*
  (fmt "TEST and TEST-NOT are designators for functions of two arguments~@
        that return a generalized boolean indicating whether the test passed.~@
        The default if neither TEST nor TEST-NOT is given is a TEST of EQL."))

(defparameter *bounding-indexes*
  (fmt "START and END are bounding index designators.  They determine~@
        an interval within SEQUENCE that is considered.  This interval contains~@
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
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a proper sequence.
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For COPY-SEQ this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a circular list or a dotted list."))

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
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is a dotted list.~@
              No error is signaled if SEQUENCE is a circular list, and then the~@
              computation will not halt, so no value is returned.~@
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
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of tyep TYPE-ERROR is signaled if SEQUENCE is a dotted list~@
              and the interval designated by START and END contains the last CONS cell~@
              of SEQUENCE.
              No error is signaled if SEQUENCE is a dotted list such that the interval~@
              designated by START and END does not contain the last CONS cell of SEQUENCE.~@
              No error is signaled if SEQUENCE is a circular list.~@
              ~@
              ~a~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For SUBSEQ this means in practice that an implementation might signal~@
              an error for circular lists, or for dotted lists such that the interval~@
              designated by START and END does not contain the last CONS cell of~@
              SEQUENCE, and that an implementation might not signal an error when~@
              SEQUENCE is a dotted list such that the interval designated by START~@
              and END contains the last CONS cell of SEQUENCE."
             *sequence*
             *maybe-error-bounding-indexes*))

(fundoc 'map
        (fmt "Lambda list: (RESULT-TYPE FUNCTION &rest SEQUENCES)~@
              ~@
              Description:~@
              Applies FUNCTION to successive elements, starting with the first one,~@
              of each sequence in SEQUENCES in such a way that the first argument~@
              to FUNCTION is taken from the first sequence, the second arguement is~@
              taken from the second sequence, and so on.~@
              FUNCTION is applied as many times as there are elements in the shortest~@
              sequence of SEQUENCES.~@
              If RESULT-TYPE is NIL, then NIL is returned.  In this case, MAP is used~@
              only for side effects.~@
              If RESULT-TYPE is not NIL, then the result is a sequence of the type~@
              specified in RESULT-TYPE containing the results of applying FUNCTION as~@
              mentioned above in the order from the first to the last application.~@
              ~@
              Arguments:~@
              RESULT-TYPE is type specifier that specifies a sequence type.~@
              FUNCTION is a designator for a function that must accept to be~@
              passed as many arguments as there are sequences.~@
              SEQUENCES is a non-empty list of proper sequences, so a call to~@
              MAP must pass at least one proper sequence in addition to the~@
              required arguments.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE is neither a~@
              recognizable subtype of LIST, a recognizable subtype of VECTOR, nor NIL.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE specifies a~@
              size and that size is different from the length of the shortest sequence~@
              in SEQUENCES.~@
              An error of type TYPE-ERROR is signaled if any of the sequences in SEQUENCES~@
              is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if any of the sequences in SEQUENCES~@
              is a dotted list, and the CAR of the last CONS cell of that list is required~@
              to compute the result.~@
              No error is signaled because any of the sequences in SEQUENCES is a circular list.~@
              No error is signaled for a sequence in SEQUENCES that is a dotted list that is~@
              so long that the CAR of the last CONS cell is not needed to compute the result.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if any sequence in SEQUENCES is not~@
              a proper sequence.  In practice this means that an implementation might~@
              not signal an error if a result can be determined without doing so~@
              or that an implementation might always signal an error when a sequence~@
              is a dotted or a circular list."))

(fundoc 'map-into
        (fmt "Lambda list: (RESULT-SEQUENCE FUNCTION &rest SEQUENCES)~@
              ~@
              Description:~@
              Applies FUNCTION to successive elements, starting with the first one,~@
              of each sequence in SEQUENCES in such a way that the first argument~@
              to FUNCTION is taken from the first sequence, the second arguement is~@
              taken from the second sequence, and so on.~@
              If there are no sequences in SEQUENCES, then FUNCTION is called with~@
              no arguments.~@
              ~@
              FUNCTION is applied as many times as there are elements in the shortest~@
              sequence of any of the sequences in SEQUENCES and RESULT-SEQUENCE~@
              The result of applying FUNCTION is stored in the successive elements~@
              of RESULT-SEQUENCE.
              ~@
              If RESULT-SEQUENCE is a VECTOR with a fill-pointer, then the fill-pointer~@
              is not taken into account when determining the shortest sequence, and~@
              RESULT-SEQUENCE is instead considered to have as many elements as indicated~@
              by (ARRAY-DIMENSION RESULT-SEQUENCE 0).  The fill pointer is then set by~@
              MAP-INTO to the number of times FUNCTION was applied.~@
              ~@
              Arguments:~@
              RESULT-SEQUENCE is a proper sequence.~@
              FUNCTION is a designator for a function that must accept to be~@
              passed as many arguments as there are sequences in SEQUENCES~@
              SEQUENCES is a (possibly empty) list of proper sequences.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if any of the sequences in SEQUENCES~@
              is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if RESULT-SEQUENCE is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if any of the sequences in SEQUENCES~@
              is a dotted list, and the CAR of the last CONS cell of that list is required~@
              to compute the result.~@
              An error of type TYPE-ERROR is signaled if RESULT-SEQUENCE is a dotted list,~@
              and a value is stored in the CAR of the last CONS cell of RESULT-SEQUENCE as~@
              a result of the successive applications of FUNCTION.~@
              No error is signaled because any of the sequences in SEQUENCES is a circular list.~@
              No error is signaled because RESULT-SEQUENCE is a circular list.~@
              No error is signaled for a sequence in SEQUENCES that is a dotted list that is~@
              so long that the CAR of the last CONS cell is not needed to compute the result.~@
              No error is signaled if RESULT-SEQUENCE is a dotted list that is so long that~@
              no value is stored in the CAR of the last CONS cell as a result of~@
              successively applying FUNCTION.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if any sequence in SEQUENCES is not~@
              a proper sequence.  In practice this means that an implementation might~@
              not signal an error if a result can be determined without doing so~@
              or that an implementation might always signal an error when a sequence~@
              is a dotted or a circular list.  A similar thing holds for RESULT-SEQUENCE."))

(fundoc 'reduce
        (fmt "Lambda list: (FUNCTION SEQUENCE &key KEY START END FROM-END INITIAL-VALUE)~@
              ~@
              Description:~@
              In the normal case, the interval designated by START and END together with~@
              the INITIAL-VALUE (if given) have at least two values.  In this case,~@
              FUNCTION is first applied to the first two such values indicated, giving~@
              an initial combined value.  Then FUNCTION is applied to the combined value~@
              and the next value, giving a new combined value, and so on until there are~@
              no more values in the interval designated by START and END together with~@
              the INITIAL-VALUE.  The final combined value is then returned.~@
              ~@
              If FROM-END is false, and no INITIAL-VALUE is given, then FUNCTION is first~@
              applied to the first two elements of the interval of SEQUENCE designated~@
              by START and END, then to the combined value and the third element in the~@
              interval, and so on as described above.~@
              ~@
              If FROM-END is false, and an INITIAL-VALUE is given, then FUNCTION is first~@
              applied to the INITIAL-VALUE and the first element of the interval of SEQUENCE~@
              designated by START and END, then to the combined value and the second element~@
              in the interval, and so on as described above.~@
              ~@
              If FROM-END is true, and no INITIAL-VALUE is given, then FUNCTION is first~@
              applied to the last two elements of the interval of SEQUENCE designated~@
              by START and END, then to the combined value and the third element from the~@
              end in the interval, and so on as described above.~@
              ~@
              If FROM-END is true, and an INITIAL-VALUE is given, then FUNCTION is first~@
              applied to the INITIAL-VALUE and the last element of the interval of SEQUENCE~@
              designated by START and END, then to the combined value and the second element~@
              from the end in the interval, and so on as described above.~@
              ~@
              If KEY is supplied and is not NIL, then it is applied to the elements of~@
              SEQUENCE before FUNCTION is applied.  Then KEY is applied to each element~@
              of sequence in the interval designated by START and END exactly once, and~@
              in the order indicated by FROM-END.  If KEY is not supplied or is NIL, then~@
              the elements themselves are passed as arguments to FUNCTION.  KEY is not~@
              applied to INITIAL-VALUE.~@
              ~@
              If the interval in SEQUENCE designated by START and END is empty and~@
              INITIAL-VALUE is not given, then the result is the value of the application~@
              of FUNCTION to zero arguments.  Otherwise, FUNCTION is always applied~@
              to two arguments.~@
              ~@
              If INITIAL-VALUE is given, and the interval designated by START and END~@
              contains no elements, then INITIAL-VALUE is returned and FUNCTION is never~@
              called.  If INITIAL-VALUE is not given, and the interval designated by~@
              START and END contains exactly one element, then the result of applying KEY~@
              to that single element is returned and FUNCTION is never called.~@
              ~@
              Arguments:~@
              FUNCTION is a designator for a function that is called with two arguments,~@
              or with zero arguments if the interval designated by START and END is empty~@
              and no initial value is given.~@
              ~a~%~a~%~a~%~a~@
              INITIAL-VALUE is any object.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of tyep TYPE-ERROR is signaled if SEQUENCE is a dotted list~@
              and the interval designated by START and END contains the last CONS cell~@
              of SEQUENCE.
              No error is signaled if SEQUENCE is a dotted list such that the interval~@
              designated by START and END does not contain the last CONS cell of SEQUENCE.~@
              No error is signaled if SEQUENCE is a circular list.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For REDUCE this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a dotted list, and that an implementation~@
              might signal an error when SEQUENCE is a circular list."
             *sequence* *key* *bounding-indexes* *from-end*))

(fundoc 'reverse
        (fmt "Lambda list: (SEQUENCE)~@
              ~@
              Description:~@
              Returns a sequence of the same type as SEQUENCE, but with the elements~@
              in the reverse order compared to SEQUENCE.~@
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
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a proper sequence.
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For REVERSE this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a circular list or a dotted list."))

(fundoc 'nreverse
        (fmt "Lambda list: (SEQUENCE)~@
              ~@
              Description:~@
              Returns a sequence of the same type as SEQUENCE, but with the elements~@
              in the reverse order compared to SEQUENCE.~@
              ~@
              If SEQUENCE is a vector, then the resulting sequence is identical to~@
              SEQUENCE, and the elements have been reordered to compute the result.~@
              If SEQUENCE is a list, then the CONS cells of SEQUENCE are reordered~@
              to compute the result, and one of the CONS cells of SEQUENCE is returned.~@
              ~@
              Arguments:~@
              SEQUENCE is a proper sequence.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is a dotted list.~@
              No error is signaled if SEQUENCE is a circular list, and NREVERSE~@
              does not halt in this case.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For NREVERSE this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a dotted list, and that an implementation~@
              might signal an error when SEQUENCE is a circular list.~@
              The Common Lisp HyperSpec gives great freedom to implementations~@
              with respect to side effects.  An implementation is allowed to~@
              return a freshly allocated sequence, to modify SEQUENCE or both,~@
              but it is also allowed not to modify SEQUENCE or to return a sequence~@
              that is built from SEQUENCE or parts of it.  Portable code should always~@
              use the resulting sequence, and never refer to SEQUENCE after a call~@
              to NREVERSE."))

(fundoc 'sort
        (fmt "Lambda list: (SEQUENCE PREDICATE &key KEY)~@
              ~@
              Description:~@
              Destructively sorts SEQUENCE and returns the sorted result.~@
              ~@
              If SEQUENCE is a VECTOR, then the resulting sequence is also a VECTOR~@
              with the same actual element type as SEQUENCE.  If SEQUENCE is a LIST,~@
              then the resulting sequence is also a LIST.~@
              ~@
              If PREDICATE and KEY always terminate, then the result is a sequence that~@
              contain the same elements as SEQUENCE, but reordered in some way.  If KEY~@
              always returns the same value when called with a particular element, and if~@
              PREDICATE represents the strictly-less-than function of a total order between~@
              the elements of SEQUENCE, then the resulting sequence is sorted in increasing~@
              order with respect to that total order. For PREDICATE to represent such a~@
              function, it must always return the same value for a given pair of arguments.~@
              Furthermore, (funcall PREDICATE x y) and (funcall PREDICATE y x) must never~@
              both return true, and if both return false, then x and y are considired equal~@
              with respect to the total order.  If KEY and/or PREDICATE always terminate but~@
              violate some of the restrictions above, the resulting sequence will still~@
              contain the same elements as SEQUENCE, but reordered in some arbitrary way.~@
              ~@
              For SORT, two elements in SEQUENCE that are considered equal according~@
              to the definition above may appear in any order in the resulting sequence.~@
              In other words, the SORT function does not necessarily use a stable~@
              sorting algorithm.~@
              If preserving order between equal elements is important, use STABLE-SORT~@
              instead.~@
              ~@
              If SEQUENCE is a vector, then the resulting sequence is identical to~@
              SEQUENCE, and the elements have been reordered to compute the result.~@
              If SEQUENCE is a list, then the CONS cells of SEQUENCE are reordered~@
              to compute the result, and one of the CONS cells of SEQUENCE is returned.~@
              ~@
              Arguments:~@
              ~a~@
              PREDICATE is a designator for a function that must accept two arguments~@
              and that returns a generalized boolean.~@
              KEY is a designator for a function that must accept one argument and~@
              that is applied to the elements of SEQUENCE before PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.  The default value of KEY is NIL.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For SORT this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a dotted list, and that an implementation~@
              might signal an error when SEQUENCE is a circular list.~@
              The Common Lisp HyperSpec gives great freedom to implementations~@
              with respect to side effects.  An implementation is allowed to~@
              return a freshly allocated sequence, to modify SEQUENCE or both,~@
              but it is also allowed not to modify SEQUENCE or to return a sequence~@
              that is built from SEQUENCE or parts of it.  Portable code should always~@
              use the resulting sequence, and never refer to SEQUENCE after a call~@
              to SORT."
             *sequence*))

(fundoc 'stable-sort
        (fmt "Lambda list: (SEQUENCE PREDICATE &key KEY)~@
              ~@
              Description:~@
              Destructively sorts SEQUENCE and returns the sorted result.~@
              ~@
              If SEQUENCE is a VECTOR, then the resulting sequence is also a VECTOR~@
              with the same actual element type as SEQUENCE.  If SEQUENCE is a LIST,~@
              then the resulting sequence is also a LIST.~@
              ~@
              If PREDICATE and KEY always terminate, then the result is a sequence that~@
              contain the same elements as SEQUENCE, but reordered in some way.  If KEY~@
              always returns the same value when called with a particular element, and if~@
              PREDICATE represents the strictly-less-than function of a total order between~@
              the elements of SEQUENCE, then the resulting sequence is sorted in increasing~@
              order with respect to that total order. For PREDICATE to represent such a~@
              function, it must always return the same value for a given pair of arguments.~@
              Furthermore, (funcall PREDICATE x y) and (funcall PREDICATE y x) must never~@
              both return true, and if both return false, then x and y are considired equal~@
              with respect to the total order.  If KEY and/or PREDICATE always terminate but~@
              violate some of the restrictions above, the resulting sequence will still~@
              contain the same elements as SEQUENCE, but reordered in some arbitrary way.~@
              ~@
              For STABLE-SORT, two elements in SEQUENCE that are considered equal according~@
              to the definition above appear in the same order in the resulting sequence.~@
              In other words, the STABLE-SORT function is guaranteed to use a stable~@
              sorting algorithm.~@
              If preserving order between equal elements is not important, use SORT~@
              instead, because it is likely to have better performance.~@
              ~@
              If SEQUENCE is a vector, then the resulting sequence is identical to~@
              SEQUENCE, and the elements have been reordered to compute the result.~@
              If SEQUENCE is a list, then the CONS cells of SEQUENCE are reordered~@
              to compute the result, and one of the CONS cells of SEQUENCE is returned.~@
              ~@
              Arguments:~@
              ~a~@
              PREDICATE is a designator for a function that must accept two arguments~@
              and that returns a generalized boolean.~@
              KEY is a designator for a function that must accept one argument and~@
              that is applied to the elements of SEQUENCE before PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.  The default value of KEY is NIL.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec states that an implementation should be~@
              prepeared to signal an error if SEQUENCE is not a proper sequence.~@
              For SORT this means in practice that an implementation might not signal~@
              an error when SEQUENCE is a dotted list, and that an implementation~@
              might signal an error when SEQUENCE is a circular list.~@
              The Common Lisp HyperSpec gives great freedom to implementations~@
              with respect to side effects.  An implementation is allowed to~@
              return a freshly allocated sequence, to modify SEQUENCE or both,~@
              but it is also allowed not to modify SEQUENCE or to return a sequence~@
              that is built from SEQUENCE or parts of it.  Portable code should always~@
              use the resulting sequence, and never refer to SEQUENCE after a call~@
              to STABLE-SORT."
             *sequence*))

(fundoc 'search
        (fmt "Lambda list: (SEQUENCE-1 SEQUENCE-2 &key KEY TEST TEST-NOT START1 START2 END1 END2 FROM-END)~@
              ~@
              Description:~@
              The interval of SEQUENCE-2 designated by START2 and END2 is searched for~@
              a subsequence that matches the subsequence defined by the interval of~@
              SEQUENCE-1 designated by START1 and END1.  If a match is found, then the~@
              position in SEQUENCE-2 of the first element of the matching subsequence~@
              is returned. Otherwise NIL is returned. A match is considered to exist when~@
              the test is satisfied for each pair of elements of the two subsequences.~@
              ~@
              If a match exists and FROM-END is true, then the position of the last~@
              matching subsequence in SEQUENCE-2 is returned.  If a match exists and~@
              FROM-END is false, then the position of the first matching subsequence~@
              in SEQUENCE-2 is returned.
              ~@
              The elements of the designated intervals of the two sequence may be tested~@
              in any order and any number of times, independently of whether FROM-END is~@
              true or false.
              ~@
              Arguments:~@
              SEQUENCE-1 is a proper sequence.~@
              SEQUENCE-2 is a proper sequence.~@
              KEY is a designator for a function that must accept one argument and~@
              that is applied to the elements of SEQUENCE-1 and of SEQUENCE-2 before~@
              the test is applied, or KEY could be NIL which means IDENTITY.~@
              The default value of KEY is NIL.~@
              ~a~@
              START1 and END1 are bounding index designators.  They determine~@
              an interval within SEQUENCE-1 that is considered.  This interval contains~@
              the indexes i such that START1 <= i < END1.  The default for START1 is 0,~@
              and the default for END1 is NIL, which means the end of the sequence.~@
              START2 and END2 are bounding index designators.  They determine~@
              an interval within SEQUENCE-2 that is considered.  This interval contains~@
              the indexes i such that START2 <= i < END2.  The default for START2 is 0,~@
              and the default for END2 is NIL, which means the end of the sequence.~@
              ~a~@
              ~@
              Satisfying the test:~@
              To determine whether a pair of elements satisfies the test,  The KEY~@
              function is first applied to the element of SEQUENCE-1 and to the element~@
              of SEQUENCE-2 to be tested.  The result is then used in the test.~@
              Then, if TEST is given, TEST is applied to the result of applying the KEY~@
              function to the two elements, with the element from SEQUENCE-1 as its~@
              first argument, and the element of SEQUENCE-2 as its second argument.~@
              If TEST returns true, then that pair of elements satisfies the test.~@
              Otherwise that pair of elements does not satisfy the test.~@
              If instead TEST-NOT is given, then TEST-NOT is is applied to the result~@
              of applying the KEY function to the two elements, with the element from~@
              SEQUENCE-1 as its first argument, and the element of SEQUENCE-2 as its~@
              second argument. If TEST-NOT returns false, then that pair of elements~@
              satisfies the test. Otherwise that pair of elements does not satisfy~@
              the test.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is a dotted list~@
              and the last CONS cell of SEQUENCE-1 is needed to compute the result.~@
              No error is signaled if SEQUENCE-1 is a dotted list and the last CONS cell~@
              of SEQUENCE-1 is not needed to compute the result.~@
              No error is signaled if SEQUENCE-1 is a circular list.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is a dotted list~@
              and the last CONS cell of SEQUENCE-2 is needed to compute the result.~@
              No error is signaled if SEQUENCE-2 is a dotted list and the last CONS cell~@
              of SEQUENCE-2 is not needed to compute the result.~@
              No error is signaled if SEQUENCE-2 is a circular list.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec does not explicity mention that SEQUENCE-1 and~@
              SEQUENCE-2 must be proper sequences, and only says that they have to be~@
              sequences.  However, section 17.1.1 states that they still have to be~@
              proper sequences.~@
              The Common Lisp HyperSpec does not specify that the test is applied with~@
              an element from SEQUENCE-1 as its first argument, and an element from~@
              SEQUENCE-2 as its second argument, though that is probably what is intended.~@
              To be completely portable, code should only use tests that are commutative~@
              and side-effect free.~@
              The Common Lisp HyperSpec does not state any exceptional situations for~@
              SEARCH.  This omission implicitly means that the behavior is undefined~@
              if SEQUENCE-1 and SEQUENCE-2 are not both proper sequences.  For that~@
              reason, portable code should not assume that any errors are signaled if~@
              this restriction is violated.~@
              Simlarly, this omission means that the behavior is undefined if the~@
              bounding index designators for any of the sequences are not valid~@
              or if some of the other arguments are not of the type indicated."
             *test-test-not* *from-end*))

(fundoc 'mismatch
        (fmt "Lambda list: (SEQUENCE-1 SEQUENCE-2 &key KEY TEST TEST-NOT START1 START2 END1 END2 FROM-END)~@
              ~@
              Description:~@
              The interval in SEQUENCE-1 designated by START1 and END1 and the interval~@
              in SEQUENCE-2 designated by START2 and END2 are compared element-wise.~@
              If the two intervals have the same length and every pair of elements satisfies~@
              the test, then there is no mismatch and NIL is returned.  Otherwise, a position~@
              of SEQUENCE-1 is returned.~@
              ~@
              If FROM-END is false, the elements of the two designated intervals are compared~@
              from the begining to the end, so that the first element of each interval are~@
              compared first.  If there is a mismatch, then the position in SEQUENCE-1~@
              of the first element that does not satisfy the test is returned.~@
              If FROM-END is true, the elements of the two designated intervals are compared~@
              from the end to the beginning, so that the last element of each interval are~@
              compared first.  If there is a mismatch, then one plus the position in SEQUENCE-1~@
              of the last element that does not satisfy the test is returned.~@
              ~@
              Arguments:~@
              SEQUENCE-1 is a proper sequence.~@
              SEQUENCE-2 is a proper sequence.~@
              KEY is a designator for a function that must accept one argument and~@
              that is applied to the elements of SEQUENCE-1 and of SEQUENCE-2 before~@
              the test is applied, or KEY could be NIL which means IDENTITY.~@
              The default value of KEY is NIL.~@
              ~a~@
              START1 and END1 are bounding index designators.  They determine~@
              an interval within SEQUENCE-1 that is considered.  This interval contains~@
              the indexes i such that START1 <= i < END1.  The default for START1 is 0,~@
              and the default for END1 is NIL, which means the end of the sequence.~@
              START2 and END2 are bounding index designators.  They determine~@
              an interval within SEQUENCE-2 that is considered.  This interval contains~@
              the indexes i such that START2 <= i < END2.  The default for START2 is 0,~@
              and the default for END2 is NIL, which means the end of the sequence.~@
              ~a~@
              ~@
              Satisfying the test:~@
              To determine whether a pair of elements satisfies the test,  The KEY~@
              function is first applied to the element of SEQUENCE-1 and to the element~@
              of SEQUENCE-2 to be tested.  The result is then used in the test.~@
              Then, if TEST is given, TEST is applied to the result of applying the KEY~@
              function to the two elements, with the element from SEQUENCE-1 as its~@
              first argument, and the element of SEQUENCE-2 as its second argument.~@
              If TEST returns true, then that pair of elements satisfies the test.~@
              Otherwise that pair of elements does not satisfy the test.~@
              If instead TEST-NOT is given, then TEST-NOT is is applied to the result~@
              of applying the KEY function to the two elements, with the element from~@
              SEQUENCE-1 as its first argument, and the element of SEQUENCE-2 as its~@
              second argument. If TEST-NOT returns false, then that pair of elements~@
              satisfies the test. Otherwise that pair of elements does not satisfy~@
              the test.~@
              ~@
              Exceptional situations:~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec does not explicity mention that SEQUENCE-1 and~@
              SEQUENCE-2 must be proper sequences, and only says that they have to be~@
              sequences.  However, section 17.1.1 states that they still have to be~@
              proper sequences.~@
              The Common Lisp HyperSpec does not specify that the test is applied with~@
              an element from SEQUENCE-1 as its first argument, and an element from~@
              SEQUENCE-2 as its second argument, though that is probably what is intended.~@
              To be completely portable, code should only use tests that are commutative~@
              and side-effect free.~@
              The Common Lisp HyperSpec does not state any exceptional situations for~@
              SEARCH.  This omission implicitly means that the behavior is undefined~@
              if SEQUENCE-1 and SEQUENCE-2 are not both proper sequences.  For that~@
              reason, portable code should not assume that any errors are signaled if~@
              this restriction is violated.~@
              Simlarly, this omission means that the behavior is undefined if the~@
              bounding index designators for any of the sequences are not valid~@
              or if some of the other arguments are not of the type indicated."
             *test-test-not* *from-end*))

(fundoc 'replace
        (fmt "Lambda list: (SEQUENCE-1 SEQUENCE-2 &key START1 START2 END1 END2)~@
              ~@
              Description:~@
              The consecutive objects in the interval of SEQUENCE-1 designated by~@
              START1 and END1 are copied into the interval of SEQUENCE-2 designated by~@
              START2 and END2, thereby modifying SEQUENCE-1.  The number of objects~@
              copied is determined by the shorter of the two intervals, which can~@
              be expressed as (MIN (- END2 START2) (- END1 START1)).~@
              ~@
              If SEQUENCE-1 and SEQUENCE-2 are the same object, i.e., the two are EQ,~@
              and the two intervals overlap, then REPLACE works as expected, i.e.,~@
              the operation is equivalent to first copying the elements of the interval~@
              of SEQUENCE-2 designated by START2 and END2 to a different place, and then~@
              copying those elements to the interval of SEQUENCE-1 designated by~@
              START1 and END1.
              ~@
              If SEQUENCE-1 and SEQUENCE-2 are not the same object, but they nevertheless~@
              share structure between the two designated intervals, then the contents of~@
              the interval of SEQUENCE-1 designated by START1 and END1 will have~@
              unpredictable contents after the operation.~@
              ~@
              Arguments:~@
              SEQUENCE-1 is a proper sequence.~@
              SEQUENCE-2 is a proper sequence.~@
              START1 and END1 are bounding index designators.  They determine~@
              an interval within SEQUENCE-1 that is considered.  This interval contains~@
              the indexes i such that START1 <= i < END1.  The default for START1 is 0,~@
              and the default for END1 is NIL, which means the end of the sequence.~@
              START2 and END2 are bounding index designators.  They determine~@
              an interval within SEQUENCE-2 that is considered.  This interval contains~@
              the indexes i such that START2 <= i < END2.  The default for START2 is 0,~@
              and the default for END2 is NIL, which means the end of the sequence.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is a dotted list~@
              and the last CONS cell of SEQUENCE-1 is needed in the operation.~@
              No error is signaled if SEQUENCE-1 is a dotted list and the last CONS cell~@
              of SEQUENCE-1 is not needed in the operation.~@
              No error is signaled if SEQUENCE-1 is a circular list.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is a dotted list~@
              and the last CONS cell of SEQUENCE-2 is needed in the operation.~@
              No error is signaled if SEQUENCE-2 is a dotted list and the last CONS cell~@
              of SEQUENCE-2 is not needed in the operation.~@
              No error is signaled if SEQUENCE-2 is a circular list.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec does not explicity mention that SEQUENCE-1 and~@
              SEQUENCE-2 must be proper sequences, and only says that they have to be~@
              sequences.  However, section 17.1.1 states that they still have to be~@
              proper sequences.~@
              The Common Lisp HyperSpec does not state any exceptional situations for~@
              SEARCH.  This omission implicitly means that the behavior is undefined~@
              if SEQUENCE-1 and SEQUENCE-2 are not both proper sequences.  For that~@
              reason, portable code should not assume that any errors are signaled if~@
              this restriction is violated.~@
              Simlarly, this omission means that the behavior is undefined if the~@
              bounding index designators for any of the sequences are not valid~@
              or if some of the other arguments are not of the type indicated."))

(fundoc 'concatenate
        (fmt "Lambda list: (RESULT-TYPE &rest SEQUENCES)~@
              ~@
              Description:~@
              Returns a sequence that is the concatenation of each sequence in~@
              SEQUENCES, i.e., a sequence that contains every element of every~@
              sequence in SEQUENCES in the same order as the the sequences in~@
              SEQUENCES and with the elements in the same order as in each sequence.~@
              ~@
              Each sequence in SEQUENCES is copied.  There is never any structure~@
              sharing between the resulting sequence and any of the sequences in~@
              SEQUENCES.~@
              ~@
              If RESULT-TYPE is a subtype of LIST, then the resulting sequence will~@
              be a LIST.~@
              If RESULT-TYPE is a subtype of VECTOR, then if the element type ~@
              specified for RESULT-TYPE can be determined, the element type of~@
              the resulting array is the result of upgrading that element type,~@
              or if it can be determined that the element type is unspecified~@
              then the element type of the resulting array is T.  Otherwise, an~@
              error is signaled.~@
              ~@
              Arguments:~@
              RESULT-TYPE is type specifier that specifies a sequence type.~@
              SEQUENCES is a (possibly empty) list of proper sequences.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if any sequence in SEQUENCES~@
              is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if any sequence in SEQUENCES~@
              is a dotted list.~@
              No error is signaled if any sequence in SEQUENCES is a circular list,~@
              and the computation will not halt in this case.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE is neither a~@
              recognizable subtype of LIST nor a recognizable subtype of VECTOR.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE specifies a~@
              size and that size is different from the sum of the lengths of the~@
              sequences in SEQUENCES.~@
              An error of type TYPE-ERROR is signaled if any element of any of the~@
              sequences in SEQUENCES can not be stored in the resulting sequence.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec does not explicity mention that the sequences~@
              in SEQUENCES must be proper sequences, and only says that they have to be~@
              sequences.  However, section 17.1.1 states that they still have to be~@
              proper sequences.~@
              The Common Lisp HyperSpec does not state what happens if any sequence~@
              in SEQUENCES is not a proper sequence.  This omission implicitly means~@
              that the behavior is undefined in this case.  For that reason, portable~@
              code should not assume that any errors are signaled if this restriction~@
              is violated.~@
              The Common Lisp HyperSpec states that it is an error if any element can~@
              not be stored in the resulting sequence, but that wording is not defined,~@
              so portable code should assume that the behavior is undefined in this case."))

(fundoc 'merge
        (fmt "Lambda list: (RESULT-TYPE SEQUENCE-1 SEQUENCE-2 PREDICATE &key KEY)~@
              ~@
              Description:~@
              The resulting sequence is constructed by treating SEQUENCE-1 and SEQUENCE-2~@
              as two stacks, successively popping one of the two top elements and adding it~@
              to the end of the resulting sequence.  When both stacks are empty, the resulting~@
              sequence is returned. If one of the stacks is empty, the other one is popped.~@
              If neither stack is empty, the top of the stack represented by SEQUENCE-1 is~@
              used as the first argument of PREDICATE, and the top of the stack represented by~@
              SEQUENCE-2 is used as the second argument of PREDICATE.  If PREDICATE returns true,~@
              then the stack represented by SEQUENCE-1 is popped, otherwise, the stack represented~@
              by SEQUENCE-2 is popped.~@
              ~@
              From the description above follows that the resulting sequence contains each element~@
              of SEQUENCE-1 exactly once, and each element of SEQUENCE-2 exactly once.  Furthermore,~@
              it follows that the relative order of two elements in SEQUENCE-1 is preserved in the~@
              resulting sequence, and that the relative order of two elements in SEQUENCE-1 is~@
              preserved in the resulting sequence.~@
              ~@
              Furthermore, it follows that if PREDICATE represents the strictly-less-than function~@
              of a total order between the elements of SEQUENCE-1 and SEQUENCE-2, and if SEQUENCE-1~@
              and SEQUENCE-2 are both sorted according to this total order, then the resulting~@
              sequence will also be sorted according to that order.  In addition, the merge operation~@
              is stable in this case, i.e., if an element of SEQUENCE-1 is equal to an element of~@
              SEQUENCE-2 according to this total order, then the element of SEQUENCE-1 will occur~@
              before the element of SEQUENCE-2 in the resulting sequence.  For PREDICATE to represent~@
              the strictly-less-than function of a total order, it must always return the same value~@
              for a given pair of arguments. Also, (funcall PREDICATE x y) and (funcall PREDICATE y x)~@
              must never both return true, and if both return false, then x and y are considired equal~@
              with respect to the total order.~@
              ~@
              If RESULT-TYPE is a subtype of LIST, then the resulting sequence will~@
              be a LIST.~@
              If RESULT-TYPE is a subtype of VECTOR, then if the element type ~@
              specified for RESULT-TYPE can be determined, the element type of~@
              the resulting array is the result of upgrading that element type,~@
              or if it can be determined that the element type is unspecified~@
              then the element type of the resulting array is T.  Otherwise, an~@
              error is signaled.~@
              ~@
              If PREDICATE represents the strictly-less-than function of a total order~@
              of the elements of SEQUENCE-1 and the elements of SEQUENCE-2, and both~@
              SEQUENCE-1 and SEQUENCE-2 are considered sorted according to PREDICATE~@
              then if some element in SEQUENCE-1 is equal to some element in SEQUENCE-2~@
              according to PREDICATE, then the element in SEQUENCE-1 will precede the~@
              element in SEQUENCE-2 in the resulting sequence.~@
              ~@
              If RESULT-TYPE is a subtype of LIST, then the CONS cells of any of SEQUENCE-1~@
              and SEQUENCE-2 that is a list will be used to construct the resulting sequence,~@
              thus destructively modifying it.  Thus, after a MERGE operation, the contents of~@
              such a sequence will be unpredictable, and should not be used.~@
              ~@
              Arguments:~@
              RESULT-TYPE is type specifier that specifies a sequence type.~@
              SEQUENCE-1 is a proper sequence.~@
              SEQUENCE-2 is a proper sequence.~@
              PREDICATE is a designator for a function that must accept two arguments~@
              and that returns a generalized boolean.~@
              KEY is a designator for a function that must accept one argument and~@
              that is applied to the elements of SEQUENCE-1 and of SEQUENCE-2 before~@
              the test is applied, or KEY could be NIL which means IDENTITY.~@
              The default value of KEY is NIL.~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE is neither a~@
              recognizable subtype of LIST nor a recognizable subtype of VECTOR.~@
              An error of type TYPE-ERROR is signaled if RESULT-TYPE specifies a~@
              size and that size is different from the sum of the lengths of~@
              SEQUENCE-1 and SEQUENCE-2.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-1 is a dotted list.~@
              No error is signaled if SEQUENCE-1 is a circular list.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is not a SEQUENCE.~@
              An error of type TYPE-ERROR is signaled if SEQUENCE-2 is a dotted list.~@
              No error is signaled if SEQUENCE-2 is a circular list.~@
              ~@
              Portability notes:~@
              The Common Lisp HyperSpec does not explicity mention that the SEQUENCE-1~@
              and SEQUENCE-2 must be proper sequences, and only says that they have to be~@
              sequences.  However, section 17.1.1 states that they still have to be~@
              proper sequences.~@
              The Common Lisp HyperSpec does not state what happens if SEQUENCE-1~@
              or SEQUENCE0-2 is not a proper sequence.  This omission implicitly means~@
              that the behavior is undefined in this case.  For that reason, portable~@
              code should not assume that any errors are signaled if this restriction~@
              is violated.~@
              "))

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
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of tyep TYPE-ERROR is signaled if SEQUENCE is a dotted list~@
              and the interval designated by START and END contains the last CONS cell~@
              of SEQUENCE.
              No error is signaled if SEQUENCE is a dotted list such that the interval~@
              designated by START and END does not contain the last CONS cell of SEQUENCE.~@
              No error is signaled if SEQUENCE is a circular list.~@
              ~@
              ~a~@
              ~@
              Portability note:~@
              The Common Lisp standard says that an error should be signaled whenever~@
              SEQUENCE is not a proper sequence.  We think this is an error in the~@
              standard document, as it would imply always testing for dotted lists~@
              and circular lists."
             *sequence* *key* *test-test-not* *bounding-indexes* *from-end*
             *satisfy-a-two-argument-test*
             *maybe-error-bounding-indexes*))

(fundoc 'delete-duplicates
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
              When SEQUENCE is a list, any top-level CAR or CDR of SEQUENCE might be~@
              modified in order to construct the resulting sequence.  When SEQUENCE~@
              is a vector, the length of SEQUENCE might be modified and the elements~@
              moved in order to construct the resulting sequence.~@
              ~@
              Arguments:~@
              ~a~%~a~%~a~%~a~%~a~@
              ~@
              Satisfying the test:~@
              ~a~@
              ~@
              Exceptional situations:~@
              An error of type TYPE-ERROR is signaled if SEQUENCE is not a SEQUENCE.~@
              An error of tyep TYPE-ERROR is signaled if SEQUENCE is a dotted list~@
              and the interval designated by START and END contains the last CONS cell~@
              of SEQUENCE.
              No error is signaled if SEQUENCE is a dotted list such that the interval~@
              designated by START and END does not contain the last CONS cell of SEQUENCE.~@
              No error is signaled if SEQUENCE is a circular list.~@
              ~@
              ~a~@
              ~@
              Portability note:~@
              The Common Lisp standard says that an error should be signaled whenever~@
              SEQUENCE is not a proper sequence.  We think this is an error in the~@
              standard document, as it would imply always testing for dotted lists~@
              and circular lists."
             *sequence* *key* *test-test-not* *bounding-indexes* *from-end*
             *satisfy-a-two-argument-test*
             *maybe-error-bounding-indexes*))

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
