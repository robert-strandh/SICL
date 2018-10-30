(cl:in-package #:sicl-clos)

;;; Given two specializers S1 and S2 that are not EQ, return true if
;;; and only if S1 is a sub-specializer S2.
;;;
;;; Recall that this function is used to determine whether one
;;; applicable method is more specific than another applicable method.
;;; It follows that S1 and S2 can not both be EQL specializers,
;;; because they would then either be EQ, and this function would not
;;; be called, or they would not be EQ and one of the methods would
;;; not be applicable.
;;;
;;; If S1 is an EQL specializer, then S1 is a sub-specializer of S2.
;;; If S2 is an EQL specializer, then S1 is not a sub-specializer of
;;; S2.  Otherwise, S1 and S2 are both classes.  Class S1 is a
;;; sub-specizlizer of class S2 with respect to some argument class C
;;; if and only if S1 occurs before S2 in the class precedence list of
;;; C.
;;;
;;; If both S1 and S2 are classes, since both S1 and S2 belong to
;;; applicable methods, we have already determined that C is more
;;; specific than both S1 and S2, and therefore both S1 and S2 are in
;;; the class precedence list of C,
;;;
;;; Should we ever be called with classes S1 and S2 that are not in
;;; the class precedence list of C, then the method we use (numeric
;;; comparison of the result of calling POSITION) will signal an
;;; error, which is reassuring.
(defun sub-specializer-p (specializer1 specializer2 class-of-argument)
  (cond ((typep specializer1 'eql-specializer)
         t)
        ((typep specializer2 'eql-specializer)
         nil)
        (t (let ((precedence-list (class-precedence-list class-of-argument)))
             (< (position specializer1 precedence-list)
                (position specializer2 precedence-list))))))

