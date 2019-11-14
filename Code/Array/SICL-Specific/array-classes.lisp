(cl:in-package #:sicl-array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY.
;;;
;;; It is entirely possible that these definitions are suboptimal, or
;;; even that they violate some aspect of the HyperSpec.  More though
;;; has to be given.
;;;
;;; All arrays are simple.

(defgeneric array-dimensions (array))

(defclass array (t)
  ((%dimensions :initarg :dimensions :reader array-dimensions))
  (:metaclass built-in-class))

(defclass simple-array (array)
  ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specialized arrays.

(defclass array-double-float (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-single-float (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-signed-byte-64 (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-unsigned-byte-64 (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-signed-byte-32 (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-unsigned-byte-32 (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-unsigned-byte-8 (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-bit (simple-array)
  ()
  (:metaclass built-in-class))

(defclass array-character (simple-array)
  ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaced arrays.
;;;
;;; This thinking might be wrong, but we are thinking that there are
;;; no specialized displaced arrays, because after all specialization
;;; has to do with different representation, and all displaced arrays
;;; are represented the same way,

(defclass displaced-array (array)
  ((%target :initarg :target)
   (%offset :initarg :offset))
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR.
;;;
;;; All our vectors have fill pointers. 
;;; [Should that be the case also for displaced vectors?]

(defgeneric fill-pointer (vector))
(defgeneric (setf fill-pointer) (new-fill-pointer vector))

(defclass vector (array sequence)
  ((%fill-pointer :initarg :fill-pointer :accessor fill-pointer))
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specialized vectors.

(defclass vector-double-float
    (vector array-double-float)
  ()
  (:metaclass built-in-class))

(defclass vector-single-float
    (vector array-single-float)
  ()
  (:metaclass built-in-class))

(defclass vector-signed-byte-64
    (vector array-signed-byte-64)
  ()
  (:metaclass built-in-class))

(defclass vector-unsigned-byte-64
    (vector array-unsigned-byte-64)
  ()
  (:metaclass built-in-class))

(defclass vector-signed-byte-32
    (vector array-signed-byte-32)
  ()
  (:metaclass built-in-class))

(defclass vector-unsigned-byte-32
    (vector array-unsigned-byte-32)
  ()
  (:metaclass built-in-class))

(defclass vector-unsigned-byte-8
    (vector array-unsigned-byte-8)
  ()
  (:metaclass built-in-class))

(defclass bit-vector
    (vector array-bit)
  ()
  (:metaclass built-in-class))

(defclass string
    (vector array-character)
  ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaced vectors.

(defclass displaced-vector
    (vector displaced-array)
  ()
  (:metaclass built-in-class))
