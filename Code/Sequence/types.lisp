(cl:in-package #:sicl-sequence)

(defun sequence-type-specifier-p (x)
  (and (subtypep x 'sequence)
       ;; The type SEQUENCE itself is not a valid sequence type specifier.
       (not (subtypep 'sequence x))))

(deftype sequence-type-specifier ()
  '(satisfies sequence-type-specifier-p))

;;; As of 2020, computers are predominantly 64 bit machines.  However,
;;; these machines typically don't use the full 64 bits for addressing
;;; bytes of memory.  A typical choice is to use 48-52 bits, which allows
;;; addressing more than 256 Terabyte of RAM.  We are even a bit more
;;; conservative and allow up to 2^53 bytes of addressable memory.
(defconstant +max-address-bits+ 53)

;;; With the number of bytes of memory that our machine can possibly
;;; handle, we can estimate some worst case numbers for the length of a
;;; vector or a list.
;;;
;;; In the case of vectors, the worst case scenario is a single bit vector
;;; that fills the entire available memory.  Such a vector would have eight
;;; times as many elements as there are bytes of memory on the machine.
;;;
;;; In the case of lists, the worst case scenario is a single proper list
;;; that fills the entire available memory.  Each cons cell of that list
;;; consists of two words of memory.  On a 64 bit machine, each word
;;; consists of eight bytes, so the most positive list index is one
;;; sixteenth of the number of bytes of memory.
;;;
;;; The most important property of all these types is that they are well
;;; below typical limits of MOST-POSITIVE-FIXNUM.  That means that not only
;;; are all elements of these types fixnums, but also their sums and
;;; differences.

(defconstant +most-positive-vector-length+
  (min (expt 2 (+ +max-address-bits+ 3))
       (1- array-total-size-limit)))

(defconstant +most-positive-list-length+
  (expt 2 (- +max-address-bits+ 4)))

(deftype vector-length ()
  `(integer 0 ,+most-positive-vector-length+))

(deftype list-length ()
  `(integer 0 ,+most-positive-list-length+))

(deftype vector-index ()
  `(integer 0 (,+most-positive-vector-length+)))

(deftype list-index ()
  `(integer 0 (,+most-positive-list-length+)))

;;; For the following two type definitions, we assume vectors and lists are
;;; the only kinds of sequences.  Please don't use the SEQUENCE-* types
;;; when dealing with user-defined sequences.

(deftype sequence-length ()
  `(or vector-length list-length))

(deftype sequence-index ()
  `(or vector-index list-index))
