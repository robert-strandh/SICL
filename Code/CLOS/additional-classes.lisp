(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-FUNCTION.

;;; We need for funcallable standard objects and standard functions to
;;; be called the same way.  There is a slight difficulty in order for
;;; that to happen, though.  When a funcallable standard object is
;;; allocated, since it is a standard object, two additional cells are
;;; allocated in the contents vector, namely for the object class
;;; unique number and for the class slots of the class.  When a
;;; standard function is allocated, however, the cell containing the
;;; class slots of the class is not present because standard functions
;;; are not standard objects, so they can not become obsolete, and
;;; therefore do not need this information in order to be updated.
;;; But we still want the slots of the FUNCTION class to have the same
;;; location in instances of STANDARD-FUNCTION.  To accomplish that,
;;; we add a dummy slot to standard functions that is in the same
;;; location as the class slots of the class in standard objects.  To
;;; add this dummy slot, we define a class DUMMY-SLOT-PROVIDER,
;;; containing such a slot.

(define-built-in-class dummy-slot-supplier (t)
  ((%dummy :initform nil)))

;;; It is important that the list of superclasses appear in the order
;;; that it does in this definition, because slots are allocated with
;;; locations that take this order into account.  In this case, the
;;; slot supplied by DUMMY-SLOT-SUPPLIER will occupy the first
;;; location in instances of STANDARD-FUNCTION.
(define-built-in-class standard-function (function dummy-slot-supplier)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SEQUENCE.
;;;
;;; As required by the HyperSpec, this class has T as its only
;;; superclass.

(define-built-in-class sequence (t)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LIST.
;;;
;;; As required by the HyperSpec, this class has SEQUENCE as a
;;; superclass.

(define-built-in-class list (sequence)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL.
;;;
;;; We definitely do not want to have slots in the symbol for the
;;; value returned by SYMBOL-FUNCTION and SYMBOL-VALUE, because those
;;; functions are not really symbol accessors, but rather functions
;;; that access the global environment.  And since we represent the
;;; global environment as a first-class object, that is where we
;;; associate symbols with values and function names with functions.   
;;;
;;; Whether we should have a slot in the symbol for the value returned
;;; by SYMBOL-PLIST is debatable.  Tentatively, we treat the plist the
;;; same way as we do SYMBOL-VALUE and SYMBOL-FUNCTION, i.e., we put
;;; the plist in the global environment.
;;;
;;; The only intrinsic properties left for a symbol, then, is the name
;;; and the package.

(define-built-in-class symbol (t)
  ((%name :initarg :name :reader symbol-name)
   (%package :initarg :package :reader symbol-package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NULL.

(define-built-in-class null (symbol list)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY.
;;;
;;; It is entirely possible that these definitions are suboptimal, or
;;; even that they violate some aspect of the HyperSpec.  More though
;;; has to be given.
;;;
;;; Tentatively, all specialized arrays are simple.

(define-built-in-class array (t)
  ((%dimensions :initarg :dimensions)))

(define-built-in-class simple-array (array)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specialized arrays.

(define-built-in-class array-double-float (simple-array)
  ())

(define-built-in-class array-single-float (simple-array)
  ())

(define-built-in-class array-signed-byte-64 (simple-array)
  ())

(define-built-in-class array-unsigned-byte-64 (simple-array)
  ())

(define-built-in-class array-signed-byte-32 (simple-array)
  ())

(define-built-in-class array-unsigned-byte-32 (simple-array)
  ())

(define-built-in-class array-unsigned-byte-8 (simple-array)
  ())

(define-built-in-class array-bit (simple-array)
  ())

(define-built-in-class array-character (simple-array)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaced arrays.
;;;
;;; This thinking might be wrong, but we are thinking that there are
;;; no specialized displaced arrays, because after all specialization
;;; has to do with different representation, and all displaced arrays
;;; are represented the same way,
;;;
;;; Could a displaced array also be simple?

(define-built-in-class displaced-array (array)
  ((%target :initarg :target)
   (%offset :initarg :offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR.
;;;
;;; All our vectors have fill pointers. 
;;; [Should that be the case also for displaced vectors?]

(define-built-in-class vector (array sequence)
  ((%fill-pointer :initarg :fill-pointer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specialized vectors.

(define-built-in-class vector-double-float
    (vector array-double-float)
  ())

(define-built-in-class vector-single-float
    (vector array-single-float)
  ())

(define-built-in-class vector-signed-byte-64
    (vector array-signed-byte-64)
  ())

(define-built-in-class vector-unsigned-byte-64
    (vector array-unsigned-byte-64)
  ())

(define-built-in-class vector-signed-byte-32
    (vector array-signed-byte-32)
  ())

(define-built-in-class vector-unsigned-byte-32
    (vector array-unsigned-byte-32)
  ())

(define-built-in-class vector-unsigned-byte-8
    (vector array-unsigned-byte-8)
  ())

(define-built-in-class bit-vector
    (vector array-bit)
  ())

(define-built-in-class string
    (vector array-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaced vectors.

(define-built-in-class displaced-vector
    (vector displaced-array)
  ())
