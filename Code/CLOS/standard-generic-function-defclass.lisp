(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-argument-precedence-order.html
(defgeneric generic-function-argument-precedence-order (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-declarations.html
(defgeneric generic-function-declarations (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-class.html
(defgeneric generic-function-method-class (generic-function))

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
   ;; This slot contains all the methods of the generic function,
   ;; including those that were defined as part of the DEFGENERIC
   ;; form.
   (%methods 
    :initform '() 
    :accessor generic-function-methods)
   ;; This slot contains only the methods that were defined as part of
   ;; the DEFGENERIC form.  We need it, because the Common Lisp
   ;; HyperSpec says that when a DEFGENERIC form is evaluated and the
   ;; generic function exists already, then the methods that were
   ;; added as a result of the evaluation of the DEFGENERIC form are
   ;; first removed.  That is not the case for methods defined by
   ;; separate DEFMETHOD forms.
   (%initial-methods
    :initform '()
    :accessor initial-methods)
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
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :declarations '()
   :method-class (find-class 'standard-method)))

;;  LocalWords:  DEFGENERIC defgeneric
