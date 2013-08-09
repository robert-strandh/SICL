(in-package #:sicl-array-low)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for element types.

(define-built-in-class element-type-t () ())

(define-built-in-class element-type-double-float () ())

(define-built-in-class element-type-single-float () ())

(define-built-in-class element-type-unsigned-byte-64 () ())

(define-built-in-class element-type-signed-byte-64 ())

(define-built-in-class element-type-unsigned-byte-32 () ())

(define-built-in-class element-type-signed-byte-32 ())

(define-built-in-class element-type-unsigned-byte-8 () ())

(define-built-in-class element-type-bit () ())

(define-built-in-class element-type-character () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for other array aspects.

(define-built-in-class simple-array-mixin () ())

(define-built-in-class fill-pointer-mixin ()
  ((%fill-pointer :initarg :fill-pointer :accessor fill-pointer)))

(define-built-in-class displaced-mixin ()
  ((%displaced-to
    :initarg :displaced-to
    :reader displaced-to)
   (%displaced-index-offset
    :initarg :displaced-index-offset
    :reader displaced-index-offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY.

(define-built-in-class array ()
  ((%dimensions :initarg :dimensions :reader array-dimensions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for arrays that are not vectors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for simple arrays that are not vectors.

(define-built-in-class simple-array-t
    (array simple-array-mixin element-type-t)
  ())

(define-built-in-class simple-array-double-float
    (array simple-array-mixin element-type-double-float)
  ())

(define-built-in-class simple-array-single-float
    (array simple-array-mixin element-type-single-float)
  ())

(define-built-in-class simple-array-unsigned-byte-64
    (array simple-array-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class simple-array-signed-byte-64
    (array simple-array-mixin element-type-signed-byte-64)
  ())

(define-built-in-class simple-array-unsigned-byte-32
    (array simple-array-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class simple-array-signed-byte-32
    (array simple-array-mixin element-type-signed-byte-32)
  ())

(define-built-in-class simple-array-unsigned-byte-8
    (array simple-array-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class simple-array-bit
    (array simple-array-mixin element-type-bit)
  ())

(define-built-in-class simple-array-character
    (array simple-array-mixin element-type-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for arrays that are not vectors and not simple.
;;;
;;; Since only vectors have fill pointers, an array that is not a
;;; vector and not simple is necessarily a displaced array.

(define-built-in-class displaced-array-t
    (array displaced-mixin element-type-t)
  ())

(define-built-in-class displaced-array-double-float
    (array displaced-mixin element-type-double-float)
  ())

(define-built-in-class displaced-array-single-float
    (array displaced-mixin element-type-single-float)
  ())

(define-built-in-class displaced-array-unsigned-byte-64
    (array displaced-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class displaced-array-signed-byte-64
    (array displaced-mixin element-type-signed-byte-64)
  ())

(define-built-in-class displaced-array-unsigned-byte-32
    (array displaced-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class displaced-array-signed-byte-32
    (array displaced-mixin element-type-signed-byte-32)
  ())

(define-built-in-class displaced-array-unsigned-byte-8
    (array displaced-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class displaced-array-bit
    (array displaced-mixin element-type-bit)
  ())

(define-built-in-class displaced-array-character
    (array displaced-mixin element-type-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vectors.

(define-built-in-class vector (array sequence)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for vectors that are also simple arrays.

;;; Recall that the class SIMPLE-VECTOR is not just a simple array
;;; that is also a vector, but it is also required to have an
;;; element-type of T.

(define-built-in-class simple-vector
    (vector simple-array-mixin element-type-t))

(define-built-in-class vectors-simple-array-double-float
    (vector simple-array-mixin element-type-double-float))

(define-built-in-class vector-simple-array-single-float
    (vector simple-array-mixin element-type-single-float)
  ())

(define-built-in-class vector-simple-array-unsigned-byte-64
    (vector simple-array-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class vector-simple-array-signed-byte-64
    (vector simple-array-mixin element-type-signed-byte-64)
  ())

(define-built-in-class vector-simple-array-unsigned-byte-32
    (vector simple-array-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class vector-simple-array-signed-byte-32
    (vector simple-array-mixin element-type-signed-byte-32)
  ())

(define-built-in-class vector-simple-array-unsigned-byte-8
    (vector simple-array-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class vector-simple-array-bit
    (vector simple-array-mixin element-type-bit)
  ())

(define-built-in-class vector-simple-array-character
    (vector simple-array-mixin element-type-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for displaced vectors that have no fill pointer. 

(define-built-in-class displaced-vector-t
    (vector displaced-mixin element-type-t)
  ())

(define-built-in-class displaced-vector-double-float
    (vector displaced-mixin element-type-double-float)
  ())

(define-built-in-class displaced-vector-single-float
    (vector displaced-mixin element-type-single-float)
  ())

(define-built-in-class displaced-vector-unsigned-byte-64
    (vector displaced-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class displaced-vector-signed-byte-64
    (vector displaced-mixin element-type-signed-byte-64)
  ())

(define-built-in-class displaced-vector-unsigned-byte-32
    (vector displaced-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class displaced-vector-signed-byte-32
    (vector displaced-mixin element-type-signed-byte-32)
  ())

(define-built-in-class displaced-vector-unsigned-byte-8
    (vector displaced-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class displaced-vector-bit
    (vector displaced-mixin element-type-bit)
  ())

(define-built-in-class displaced-vector-character
    (vector displaced-mixin element-type-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for vectors that are not displaced, but that have a fill
;;; pointer.

(define-built-in-class vector-with-fill-pointer-t
    (vector fill-pointer-mixin element-type-t)
  ())

(define-built-in-class vector-with-fill-pointer-double-float
    (vector fill-pointer-mixin element-type-double-float)
  ())

(define-built-in-class vector-with-fill-pointer-single-float
    (vector fill-pointer-mixin element-type-single-float)
  ())

(define-built-in-class vector-with-fill-pointer-unsigned-byte-64
    (vector fill-pointer-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class vector-with-fill-pointer-signed-byte-64
    (vector fill-pointer-mixin element-type-signed-byte-64)
  ())

(define-built-in-class vector-with-fill-pointer-unsigned-byte-32
    (vector fill-pointer-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class vector-with-fill-pointer-signed-byte-32
    (vector fill-pointer-mixin element-type-signed-byte-32)
  ())

(define-built-in-class vector-with-fill-pointer-unsigned-byte-8
    (vector fill-pointer-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class vector-with-fill-pointer-bit
    (vector fill-pointer-mixin element-type-bit)
  ())

(define-built-in-class vector-with-fill-pointer-character
    (vector fill-pointer-mixin element-type-character)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for displaced vectors that have a fill pointer.

(define-built-in-class displaced-vector-with-fill-pointer-t
    (vector fill-pointer-mixin displaced-mixin element-type-t)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-double-float
    (vector fill-pointer-mixin displaced-mixin element-type-double-float)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-single-float
    (vector fill-pointer-mixin displaced-mixin element-type-single-float)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-unsigned-byte-64
    (vector fill-pointer-mixin displaced-mixin element-type-unsigned-byte-64)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-signed-byte-64
    (vector fill-pointer-mixin displaced-mixin element-type-signed-byte-64)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-unsigned-byte-32
    (vector fill-pointer-mixin displaced-mixin element-type-unsigned-byte-32)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-signed-byte-32
    (vector fill-pointer-mixin displaced-mixin element-type-signed-byte-32)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-unsigned-byte-8
    (vector fill-pointer-mixin displaced-mixin element-type-unsigned-byte-8)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-bit
    (vector fill-pointer-mixin displaced-mixin element-type-bit)
  ())

(define-built-in-class displaced-vector-with-fill-pointer-character
    (vector fill-pointer-mixin displaced-mixin element-type-character)
  ())

