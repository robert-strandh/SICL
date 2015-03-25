(cl:in-package #:sicl-array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY.
;;;
;;; It is entirely possible that these definitions are suboptimal, or
;;; even that they violate some aspect of the HyperSpec.  More though
;;; has to be given.
;;;
;;; Tentatively, all specialized arrays are simple.

(defgeneric array-dimensions (array))

(define-built-in-class array (t)
  ((%dimensions :initarg :dimensions :reader array-dimensions)))

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

(defgeneric fill-pointer (vector))
(defgeneric (setf fill-pointer) (new-fill-pointer vector))

(define-built-in-class vector (array sequence)
  ((%fill-pointer :initarg :fill-pointer :accessor fill-pointer)))

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
