(cl:in-package #:sicl-direct-extrinsic-compiler)

;;; A STEP TABLE is a table that is used by the STEP commands of the
;;; debugger.  The value maps values of the program counter to either
;;; a singleton value of the program counter (when there is only one
;;; possible execution path, or to a list of values of the program
;;; counter (when there is more than one possible execution path).
(defclass step-table ()
  (;; This slot contains a vector of the values of the program
   ;; counter, sorted in increasing order.  To find the corresponding
   ;; entry in the table, we do a binary search of this vector to
   ;; obtain an index.  Then we use that index into the
   ;; NEXT-PROGRAM-COUNTER vector where the value is stored.
   (%current-program-counter :initarg :current-program-counter
                             :reader current-program-counter)
   (%next-program-counter :initarg :next-program-counter
                          :reader next-program-counter)))

(defclass code ()
  (;; In the direct extrinsic compiler, this slot contains a
   ;; vector with elements of type (UNSIGNED-BYTE 8).
   (%instructions :initarg :instructions :reader instructions)
   ;; This slot holds a STEP-TABLE for the STEP-IN command of the
   ;; debugger.
   (%step-in-table :initarg :step-in-table :reader step-in-table)
   ;; This slot holds a STEP-TABLE for the STEP-OVER command of the
   ;; debugger.
   (%step-over-table :initarg :step-over-table :reader step-over-table)
   ;; This slot holds a STEP-TABLE for the STEP-OUT command of the
   ;; debugger.
   (%step-out-table :initarg :step-out-table :reader step-out-table)))

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
