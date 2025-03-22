(cl:in-package #:sicl-arithmetic)

(defclass simulated-double-float (double-float standard-object)
  ((%bit-pattern :initarg :bit-pattern :reader bit-pattern)))

(defconstant most-positive-double-float
  (make-instance 'simulated-double-float
    :bit-pattern buoy-simulate::most-positive-double-float))

(defconstant most-positive-long-float
  most-positive-double-float)

(defconstant least-positive-double-float
  (make-instance 'simulated-double-float
    :bit-pattern buoy-simulate::least-positive-double-float))

(defconstant least-positive-long-float
  least-positive-double-float)

(defconstant least-positive-normalized-double-float
  (make-instance 'simulated-double-float
    :bit-pattern buoy-simulate:least-positive-normalized-double-float))

(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)  

(defconstant most-negative-double-float
  (make-instance 'simulated-double-float
    :bit-pattern buoy-simulate:most-negative-double-float))

(defconstant most-negative-long-float
  most-negative-double-float)
