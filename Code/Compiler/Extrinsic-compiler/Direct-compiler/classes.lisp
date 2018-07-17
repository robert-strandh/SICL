(cl:in-package #:sicl-direct-extrinsic-compiler)

(defclass code ()
  (;; In the direct extrinsic compiler, this slot contains a
   ;; vector with elements of type (UNSIGNED-BYTE 8).
   (%instructions :initarg :instructions :reader instructions)
   ;; This slot holds information about source location as a function
   ;; of the program counter, and information for the debugger, such
   ;; as what variables are in scope, where they are stored, and what
   ;; types they have.  We have yet to determine the exact format of
   ;; this information.
   (%debug-info :initarg :debug-info :reader debug-info)))

(defclass function ()
  (;; This slot contains an offset into the instructions vector where
   ;; the entry point of this function is located.  In the direct
   ;; extrinsic compiler, this slot currently always contains 0, but
   ;; we might add the possibility for the direct extrinsic compiler
   ;; to compile several functions simultaneously that share the same
   ;; code.  In that case, different such functions wold contain
   ;; different offsets.  In the native system, the corresponding slot
   ;; holds a fixnum, but it really means the address of the first
   ;; instruction of the function.  When the garbage collector moves
   ;; the instruction vector of the code, it also updates this slot.
   (%entry-point :initform 0 :initarg :entry-point :accessor entry-point)
   ;; The environment of a function is a list of simple vectors.  When
   ;; the direct extrinsic compiler produces a function, the list
   ;; contains a single simple vector.  That vector holds the code as
   ;; its first element.  Each remaining element holds either a
   ;; constant, or an instance of the class CELL-INFO. 
   (%environment :initarg :environment :reader environment)
   ;; This slot has no analogue slot in the target system.  It
   ;; contains a bit vector that has the same length as the vector in
   ;; the first element of the environment list.  When a bit is `1' in
   ;; this bit vector, the corresponding element of the environment
   ;; vector contains an instance of the class CELL-INFO, to be used
   ;; to link the function to a particular environment.  When the bit
   ;; is `0', the corresponding element of the environment vector
   ;; contains a literal constant that needs no action at linkage
   ;; time.  This vector is needed because it is entirely possible for
   ;; an instance of CELL-INFO to be a constant, so we need to
   ;; distinguish between those two cases.
   (%linkage-info :initarg :linkage-info :reader linkage-info)))

;;; An instance of this class is added as an element to the vector
;;; that is located in the first element of the static environment to
;;; indicate that the corresponding element should be replaced by a
;;; value cell or a function cell from the environment at link time. 
(defclass cell-info ()
  (;; Depending on whether this is an instance of VALUE-CELL-INFO or
   ;; FUNCTION-CELL-INFO, this slot either contains a a variable name
   ;; (a symbol) or a function name (either a symbol or a list of the
   ;; form (SETF <symbol>).
   (%name :initarg :name :reader name)))

;;; An instance of this class indicates that the corresponding element
;;; in the final runtime environment should be the global value cell
;;; of a variable from the global environment.
(defclass variable-cell-info (cell-info) ())

;;; An instance of this class indicates that the corresponding element
;;; in the final runtime environment should be a function cell from
;;; the global environment. 
(defclass function-cell-info (cell-info) ())
