(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data.
;;;
;;; Only Common Lisp objects are used as data in the high-level
;;; intermediate representation, but they can be BOXED or UNBOXED. 
;;;
;;; Three kinds of data are possible in the high-level intermediate
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
;;;   * VALUES-LOCATION.  This type of data can be used both as input
;;;     and output to instructions.  It holds a an arbitrary number of
;;;     Lisp values.  The number of values it holds is statically
;;;     unknown.
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
;;; This datum class is used when the initial MIR program is created
;;; for any lexical variable.  
;;; 
;;; In later compilation stages, this datum is replaced by more
;;; specific locations as a result of further analyses of the program.

(defclass lexical-location (datum)
  ((%name :initarg :name :reader name)))

(defun make-lexical-location (name)
  (make-instance 'lexical-location
    :name name))

(defmethod print-object ((object lexical-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

;;; Generate a new lexical location
(defun new-temporary ()
  (make-lexical-location (gensym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class DYNAMIC-LEXICAL-LOCATION.
;;;
;;; This datum is a special case of a LEXICAL-LOCATION.  It is used
;;; for locations that are only referred to within a single function,
;;; so that there is no possible capture.  A location of this type can
;;; be allocated in a register or on the stack.

(defclass dynamic-lexical-location (lexical-location)
  ())

(defun make-dynamic-lexical-location (name)
  (make-instance 'dynamic-lexical-location
    :name name))

;;; Generate a new lexical location
(defun new-dynamic-temporary ()
  (make-dynamic-lexical-location (gensym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class STATIC-LEXICAL-LOCATION.
;;;
;;; This datum is a special case of a LEXICAL-LOCATION.  It is used
;;; for locations that are referred to from within several functions.
;;; Whether a location of this type has dynamic extent (so that it can
;;; be allocated on the stack, or perhaps even in a register) or
;;; indefinite extent (so that it must be allocated on the heap)
;;; depends on further analyses.

(defclass static-lexical-location (lexical-location)
  ())

(defun make-static-lexical-location (name)
  (make-instance 'static-lexical-location
    :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class VALUES-LOCATION.

(defclass values-location (datum)
  ())

(defun make-values-location ()
  (make-instance 'values-location))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class LOAD-TIME-VALUE-INPUT.

(defclass load-time-value-input (datum)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-input (form &optional read-only-p)
  (make-instance 'load-time-value-input
    :form form
    :read-only-p read-only-p))
