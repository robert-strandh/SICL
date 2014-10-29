(in-package #:sicl-configuration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The size of a machine word.

(defconstant +word-size-in-bytes+ 4)
(defconstant +word-size-in-bits+ (* +word-size-in-bytes+ 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tags
;;;
;;; We define only four main tags.  Two of the tags represent
;;; immediate data (fixnum, immediate) and two represent
;;; heap-allocated data (cons, other).  
;;;
;;; Fixnums are represented with two zero tag bits.  A machine number
;;; is simply shifted two positions to the left to obtain a fixnum.
;;; In other words, a fixnum is just represented as a machine number
;;; with 4 times the magnitude.  Thus, a negative fixnum has the most
;;; significant bit set to `1'.  This representation is well known to
;;; have some good properties, in particular that addition and
;;; subtraction can be performed on fixnums using existing machine
;;; instructions, and multiplcation only reuquires a shift operation
;;; to turn the result into the right representation (unless, of
;;; course, the operations cause overflow and we have to create a
;;; bignum instead.
;;;
;;; Immediate data other than fixnums is typically characters, but
;;; floating-point numbers that require no more than the available
;;; bits can also be represented like this.  We also represent UNBOUND
;;; (i.e. a distinguished object used to fill unbound slots and
;;; variable values) as an immediate quantity.  Notice however that
;;; unbound functions are not represented this way, because they are
;;; represented by a particular function that signals an error.
;;;
;;; A CONS cell is represented as two consecutive words.  CONS cells
;;; have their own tag.
;;; 
;;; A heap allocated object other than CONS cell is called a GENERAL
;;; INSTANCE.  General instances are represented as a two-word HEADER
;;; where the first word refers to the CLASS of the instance and the
;;; second word to the RACK.  

(defconstant +tag-fixnum+    #b00)
(defconstant +tag-cons+      #b01)
(defconstant +tag-immediate+ #b10)
(defconstant +tag-general+   #b11)

(defconstant +tag-mask+      #b11)

(defconstant +tag-width+ (integer-length +tag-mask+))

(defconstant +immediate-tag-mask+ #b1111)

(defconstant +tag-character+ #b0010)

(defconstant +immediate-tag-width+ (integer-length +immediate-tag-mask+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum range and conversion.

(defconstant most-positive-fixnum
  (1- (ash 1 (- +word-size-in-bits+ +tag-width+ 1))))

(defconstant most-negative-fixnum
  (- (ash 1 (- +word-size-in-bits+ +tag-width+ 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unbound.
;;;
;;; We define the unbound value to be a machine word with immediate
;;; tag, and all the other bits equal to 1. 

(defconstant +unbound+
  (logior (logandc2 (1- (ash 1 +word-size-in-bits+)) +tag-mask+)
	  +tag-immediate+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities for CONS cells.

(defconstant +car-offset+
  (- +tag-cons+))

(defconstant +cdr-offset+
  (- +word-size-in-bytes+ +tag-cons+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities for general instances.

(defconstant +class-offset+
  (- +tag-general+))

(defconstant +rack-offset+
  (- +word-size-in-bytes+ +tag-general+))
