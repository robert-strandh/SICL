(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data.
;;;
;;; Only Common Lisp objects are used as data in the high-level
;;; intermediate representation, but they can be BOXED or UNBOXED. 
;;;
;;; Two kinds of data are possible in the high-level intermediate
;;; representation.  The first kind is called CONSTANT-INPUT and is
;;; used for any Common Lisp object considered to be literal data.
;;; The second kind is called LEXICAL-LOCATION and is used both as
;;; input and output to instructions. 
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
(defun new-temporary ()
  (make-dynamic-lexical-location (gensym)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class INDEFINITE-LEXICAL-LOCATION.
;;;
;;; This datum is a special case of a LEXICAL-LOCATION.  It is used
;;; for locations that are only referred to within several functions,
;;; so that the location has indefinite extent.  Whether a location of
;;; this type can be allocated in a register or on the stack depends
;;; on further analyses.  To be safe, client code might want to turn
;;; this location into a STATIC-LOCATION so that it is allocated in
;;; the static environment.

(defclass indefinite-lexical-location (lexical-location)
  ())

(defun make-indefinite-lexical-location (name)
  (make-instance 'indefinite-lexical-location
    :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class LOAD-TIME-INPUT.
;;;
;;; This datum typically corresponds to a LOAD-TIME-VALUE in source
;;; code.
;;;
;;; A datum of this type contains code to be run when the compiled
;;; file is loaded.

(defclass load-time-input (datum)
  (;; The first instruction of an instruction graph that, when
   ;; called, returns the value to put into the linkage vector.
   (%initial-instruction
    :initarg :initial-instruction
    :reader initial-instruction)))

(defun make-load-time-input (initial-instruction)
  (make-instance 'load-time-input
    :initial-instruction initial-instruction))

