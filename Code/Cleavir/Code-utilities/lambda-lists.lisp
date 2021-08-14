(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda list utilities.

;;; There is much to say about lambda lists.
;;;
;;; There are 10 different types of lambda lists and they vary both
;;; with respect to syntax and semantics.  It gets pretty messy in
;;; fact.
;;;
;;; Lambda lists admit something known as "lambda list keywords".
;;; They are not keywords in the normal sense of the word (i.e.,
;;; symbols in the :KEYWORD package), but just ordinary symbols in
;;; the COMMON-LISP package that happen to have names that start with
;;; the `&' character (ampersand).
;;;
;;; The lambda list keywords that are allowed in each different type
;;; of lambda list are clearly indicated in the CLHS for that type of
;;; lambda list.  However, the CLHS also allows for
;;; implementation-specific lambda list keywords.  The complete list
;;; of lambda list keywords that a particular implementation
;;; recognizes is available as the value of the variable
;;; LAMBDA-LIST-KEYWORDS.  However, there is no way to determine what
;;; an implementation-specific lambda list keyword means, nor how it
;;; is used or even in which type of lambda list it is allowed.  There
;;; is also no indication as to whether implementation-specific lambda
;;; list keywords must begin with `&'.
;;;
;;; The lambda list keywords that are recognized by the CLHS are:
;;; &allow-other-keys, &aux, &body, &environment, &key, &optional,
;;; &rest, and &whole.  The lambda list keywords &body and &rest are
;;; synonymous, but good style gives preference to one rather than the
;;; other according to the type of lambda list it occurs in.
;;;
;;; To make things more complicated, the CLHS does not tell us how to
;;; handle occurrences of a particular lambda list keyword in a lambda
;;; list of a type that does not recognize it.  One interpretation
;;; could be that such a lambda list keyword should be treated as just
;;; any symbol, so that it becomes the name of a parameter.  But with
;;; this interpretation, a program can have some subtle bugs just
;;; because a programmer incorrectly believes that a particular
;;; lambda-list keyword is acceptable in a type of lambda list where
;;; in fact it is not.  According to another interpretation, a
;;; program-error should be signaled in this case.  At the very least,
;;; it seems reasonable to give a style-warning in that case.
;;;
;;; Similarly, the CLHS does not indicate how to handle occurrences of
;;; symbols that do not occur in LAMBDA-LIST-KEYWORDS, but that happen
;;; to start with `&'.  Again, some subtle bugs could result if such a
;;; situation were not to be flagged to the programmer.  Again, at the
;;; very least, a style warning seems to be appropriate.
;;;
;;; Lambda list keywords have different arities, i.e., the number of
;;; items that can follow it in a lambda list.  The good news is that
;;; each lambda list keyword has the same arity no matter what type of
;;; lambda list it occurs in.  Thus &allow-other-keys always has arity
;;; 0 (zero), &rest, &body, &whole, and &environment always have arity
;;; 1 (one), and the remaining ones (&aux, &key, and &optional) can
;;; take any number of items, so have arbitrary arity.
;;;
;;; Another piece of relatively good news is that the order in which
;;; lambda list keywords can occur in a lambda list is independent of
;;; the type of lambda list in which they occur, and that the relative
;;; order between two lambda list keywords is fixed, with &environment
;;; being the only exception, because it can occur anywhere (except
;;; before &whole) in the lambda lists in which it is allowed.
;;;
;;; A piece of not-so-good news is that &whole, whenever it is
;;; allowed, must appear first in the lambda list.  That is, not only
;;; first as in the first lambda list keyword, but as the first item
;;; in the lambda list, before the list of required variables.  This
;;; rule messes up syntax checking a bit.
