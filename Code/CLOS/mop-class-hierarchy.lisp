(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Metaobject hierarchy.

;;; The Metaobject hierarchy according to the AMOP table 5.1.
;;;
;;; Metaobject class                   Direct superclasses
;;; ----------------                   -------------------
;;; standard-object                    (t)
;;; funcallable-standard-object        (standard-object function)
;;; metaobject                         (standard-object)
;;; generic-function                   (metaobject
;;;                                     funcallable-standard-object)
;;; standard-generic-function          (generic-function)
;;; method                             (metaobject)
;;; standard-method                    (method)
;;; standard-accesssor-method          (standard-method)
;;; standard-reader-method             (standard-accesssor-method)
;;; standard-writer-method             (standard-accesssor-method)
;;; method-combination                 (metaobject)
;;; slot-definition                    (metaobject)
;;; direct-slot-definition             (slot-definition)
;;; effective-slot-definition          (slot-definition)
;;; standard-slot-definition           (slot-definition)
;;; standard-direct-slot-definition    (standard-slot-definition 
;;;                                     direct-slot-definition)
;;; standard-effective-slot-definition (standard-slot-definition 
;;;                                     effective-slot-definition)
;;; specializer                        (metaobject)
;;; eql-specializer                    (specializer)
;;; class                              (specializer)
;;; built-in-class                     (class)
;;; forward-referenced-class           (class)
;;; standard-class                     (class)
;;; funcallable-standard-class         (class)
;;; 
;;; The class t is an instance of built-in-class.
;;;
;;; The classes generic-function and standard-generic-function
;;; are instances of funcallable-standard-class.
;;;
;;; All other classes are instances of standard-class.

;;; For performance reasons, it is important to order the class
;;; definitions in this file in depth-first pre-order.  During
;;; bootstrapping, this is the order in which the classes will be
;;; finalized, and finalization is what assigns unique class numbers.
;;; The depth-first pre-order ensures that the classes in a subtree
;;; have consecutive class numbers, and during generic function
;;; dispatch, the implications are that membership in an entire
;;; subtree can be tested with at most 2 tests.  This property makes
;;; it cheap to define common superclasses and methods specialized to
;;; such classes.  For classes and generic functions defined at the
;;; application level, this additional performance may not make much
;;; difference, but for system-level classes and generic functions, it
;;; may significantly improve overall performance.

(defparameter *class-unique-number* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MOP class hierarchy. 

(defconstant +class-unique-number-offset+ 0)
(defconstant +instance-slots-offset+ 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class T.

(define-built-in-class t () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION.

(define-built-in-class function (t)
  ((%entry-point :initarg :entry-point)
   (%linkage-rack :initarg :linkage-rack)
   (%environment :initform nil :initarg :environment)))
