(cl:in-package #:cleavir-ir)

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
;;; Datum class WORD-INPUT.
;;;
;;; The WORD-INPUT datum corresponds to a raw machine interger that
;;; may or may not be sufficiently small to occur directly in the
;;; instruction stream.  The machine integer is represented in the
;;; instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character.
;;;
;;; Data of this type are introduced in early compilation stages
;;; when a WORD-AST is compiled, and in later stages when
;;; backend-specific code determins that a Lisp constant has a
;;; representation as a machine word, typically a character constant.
;;; Notice that such as character constant may or may not be possible
;;; to represent as an immediate input, depending on the Unicode code
;;; point of the character, and depending on the magnitude of
;;; immediates that the bacend can handle.

(defclass word-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-word-input (value)
  (make-instance 'word-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class EXTERNAL-INPUT.
;;;
;;; This datum corresponds to an index in the linkage vector of a code
;;; object.  Later compilation stages typically replace a
;;; CONSTANT-INPUT that can not be an IMMEDIATE-INPUT by an
;;; EXTERNAL-INPUT.

(defclass external-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-external-input (value)
  (make-instance 'external-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class REGISTER-LOCATION.
;;;
;;; This datum corresponds to a processor register.  It is
;;; introduced by the register allocation phase.

(defclass register-location (datum)
  ((%name :initarg :name :reader name)))

(defun make-register-location (name)
  (make-instance 'register-location
    :name name))

(defmethod print-object ((object register-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class STATIC-LOCATION.
;;;
;;; This datum corresponds to a places in the static runtime
;;; envirionment.  That environment is organized in LAYERS, with each
;;; layer being a vector of values.  

(defclass static-location (datum)
  ((%layer :initarg :layer :reader layer)
   (%index :initarg :index :reader index)))

(defun make-static-location (layer index)
  (make-instance 'static-location
    :layer layer
    :index index))

