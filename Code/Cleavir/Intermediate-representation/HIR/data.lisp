(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data.
;;;
;;; Only Common Lisp objects are used as data in the high-level
;;; intermediate representation, but they can be BOXED or UNBOXED. 
;;;
;;; Two kinds of data are possible in the high-level intermediate
;;; representation:
;;;
;;;   * CONSTANT-INPUT.  This type of data can only be used as input
;;;     to instructions.  It is used for any Common Lisp object
;;;     considered to be literal data.
;;;
;;;   * LEXICAL-LOCATION.  This type of data can be used both as input
;;;     and output to instructions.  It holds a single Lisp datum, but
;;;     that datum can be BOXED or UNBOXED.
;;;
;;; An instruction I REFERS TO a lexical location L if and only if L
;;; is either one of the inputs or one of the outputs of I.
;;;
;;; A lexical location can be referred to by several different
;;; instructions that belong to procedures at different nesting
;;; depths.  Because of the way lexical locations are created, if a
;;; lexical location is referred to by two different instructions
;;; belonging to two different procedures, P and Q, and neither P is
;;; nested inside Q nor is Q nested inside P, then the lexical
;;; location is also referred to by some instruction belonging to a
;;; procedure C inside which both A and B are nested.
;;;
;;; A lexical location L is said to be PRESENT in a procedure P if and
;;; only if some instruction belonging to P refers to L.  A lexical
;;; location L is said to BELONG to a procedure P if L is present in
;;; P, and L is not present in a procedure inside which P is nested.
;;; Because of the restriction in the previous paragraph, every
;;; lexical location belongs to some unique procedure.  The procedure
;;; P to which a lexical location belongs is called the OWNER of the
;;; lexical location.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class CONSTANT-INPUT.

(defclass constant-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-constant-input (value)
  (make-instance 'constant-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class LEXICAL-LOCATION.
;;;
;;; This datum class is used when the initial HIR program is created
;;; for any lexical variable.  

(defclass lexical-location (datum)
  ((%name :initarg :name :reader name)))

(defun make-lexical-location (name &optional (class 'lexical-location))
  (make-instance class :name name))

(defmethod print-object ((object lexical-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

(defmethod element-type ((object lexical-location))
  't)

;;; Generate a new lexical location
(defun new-temporary (&optional (thing nil thing-p) (class 'lexical-location))
  (make-lexical-location (if thing-p (gensym thing) (gensym))
                         class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class RAW-DATUM.
;;;
;;; This class is the base class for all raw data.  It contains a size
;;; attribute that determines the number of bits that this datum
;;; consists of.

(defclass raw-datum (lexical-location)
  ((%size :initarg :size :reader size))
  (:default-initargs :name (gensym "RAW")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class RAW-INTEGER.

(defclass raw-integer (raw-datum)
  ())

(defmethod element-type ((raw raw-integer))
  't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class RAW-FLOAT.

(defclass raw-float (raw-datum)
  ())

(defmethod element-type ((raw raw-float))
  'double-float)

;;; Return a class specifier for a subclass of RAW-DATUM which could
;;; store a value of the given type.
(defun raw-datum-class-for-type (type)
  (cond
    ((member type '(single-float double-float))
     'cleavir-ir:raw-float)
    ((subtypep type '(or (signed-byte 64) (unsigned-byte 64)))
     'cleavir-ir:raw-integer)
    (t
     (error "What location class would a ~s fit in?" type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class IMMEDIATE-INPUT.
;;;
;;; The IMMEDIATE-INPUT datum corresponds to a raw machine interger
;;; that is considered sufficiently small that it can occur directly
;;; in the instruction stream.  The machine integer is represented in
;;; the instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character.
;;;
;;; Data of this type are introduced by backend-specific code,
;;; because whether or not some datum can be represented as immediate
;;; input depends on the backend.

(defclass immediate-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-immediate-input (value)
  (make-instance 'immediate-input
    :value value))
