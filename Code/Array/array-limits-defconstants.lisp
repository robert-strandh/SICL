(cl:in-package #:sicl-array)

;;; The elements of an array that consists of double float complex
;;; numbers, uses 16 = 2^4 bytes per element.  So if the address space
;;; consists of 2^64 bytes, we can have at most 2^60 elements.  Then
;;; we must subtract some for the class metaobject, the header, etc.
;;; And there must be room for the system itself.  So let's say to be
;;; safe, we do 2^59.
(defconstant array-total-size-limit
  (expt 2 59))

(defconstant array-dimension-limit
  (expt 2 59))

(defconstant array-rank-limit
  (expt 2 59))
