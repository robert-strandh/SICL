(cl:in-package #:sicl-clos)

(defclass standard-generic-function (generic-function)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader generic-function-argument-precedence-order)
   (%declarations 
    :initarg :declarations
    :reader generic-function-declarations)
   (%method-class 
    :initarg :method-class
    :reader generic-function-method-class)
   (%method-combination 
    :initarg :method-combination
    ;; FIXME: remove this later.
    :initform nil
    :reader generic-function-method-combination)
   (%methods 
    :initform '() 
    ;; This reader is the one that the AMOP specifies.
    :reader generic-function-methods
    :writer (setf methods))
   ;; We maintain a CALL HISTORY of the generic function.  This call
   ;; history is a list of call records.  Whenever a call is made to
   ;; the generic function with some call profile that has not yet
   ;; been used in a call, we compute the effective method to use, and
   ;; we add a call record to the call history.
   (%call-history 
    :initform '() 
    :accessor call-history)
   ;; This slot is set by ADD-METHOD and REMOVE-METHOD.
   ;; It cnotains a list that has the same length as the 
   ;; number of required parameters of the generic function.
   ;; It contains NIL in each position where only the class T
   ;; is specialized for, and T in each position where some
   ;; method specialized for something other than the class T.
   (%specializer-profile
    :initarg :specializer-profile
    :accessor specializer-profile))
  (:metaclass funcallable-standard-class))
