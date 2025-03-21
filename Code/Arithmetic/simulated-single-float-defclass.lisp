(cl:in-package #:sicl-arithmetic)

;;; During bootstrapping, we can't use floating-point objects from the
;;; host, and we don't have SICL floating-point arithmetic in place
;;; either.  So we need to simulate what is needed during
;;; bootstrapping.  In SICL we have the two classes SINGLE-FLOAT and
;;; DOUBLE-FLOAT.  DOUBLE-FLOAT is a subclass of STANDARD-OBJECT so it
;;; has a slot that in the native system contains the bit pattern for
;;; an IEEE 754 double-precision floating-point value.  During
;;; bootstrapping, we will store that bit pattern in a host integer
;;; instead.  SINGLE-FLOAT, on the other hand, is a built-in class,
;;; because we store single floats as tagged immediate values, so we
;;; have no object to store the bit pattern in.  For that reason,
;;; during bootstrapping, we simulate SINGLE-FLOAT instances with
;;; instances of the class defined here.  The slot contains the bit
;;; pattern of an IEEE 754 single-precision floating point value
;;; represented as a host integer.

(defclass simulated-single-float (single-float standard-object)
  ((%bit-pattern :initarg :bit-pattern :reader bit-pattern)))

(defconstant most-positive-single-float
  (make-instance 'simulated-single-float
    :bit-pattern buoy-simulate::most-positive-single-float))

(defconstant most-positive-short-float
  most-positive-single-float)

(defconstant least-positive-single-float
  (make-instance 'simulated-single-float
    :bit-pattern buoy-simulate::least-positive-single-float))

(defconstant least-positive-short-float
  least-positive-single-float)

(defconstant least-positive-normalized-single-float
  (make-instance 'simulated-single-float
    :bit-pattern buoy-simulate:least-positive-normalized-single-float))

(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)  

(defconstant most-negative-single-float
  (make-instance 'simulated-single-float
    :bit-pattern buoy-simulate:most-negative-single-float))

(defconstant most-negative-short-float
  most-negative-single-float)
