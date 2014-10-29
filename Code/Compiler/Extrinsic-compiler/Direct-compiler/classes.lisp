(in-package #:sicl-direct-extrinsic-compiler)

(defclass code ()
  (;; In the direct extrinsic compiler, this slot contains a simple
   ;; vector holding non-negative integers between 0 and 255.  We use
   ;; a simple vector because the host implementation might not have
   ;; any appropriate specialized vector.
   (%instructions :initarg :instructions :reader instructions)
   ;; This slot holds information about source location as a function
   ;; of the program counter, and information for the debugger, such
   ;; as what variables are in scope, where they are stored, and what
   ;; types they have.  We have yet to determine the exact format of
   ;; this information.
   (%debug-info :initarg :debug-info :reader debug-info)))

(defclass function ()
  (;; In the direct extrinsic compiler, this slot always contains 0.
   ;; In the native system, it holds a fixnum, but it really means the
   ;; address of the first instruction of the function.  When the
   ;; garbage collector moves the instruction vector of the code, it
   ;; also updates this slot.
   (%entry-point :initform 0 :initarg :entry-point :accessor entry-point)
   ;; The environment of a function is a list of simple vectors.  When
   ;; the direct extrinsic compiler produces a function, the list
   ;; contains a single simple vector.  That vector holds the code as
   ;; its first element.  Remaining elements hold constants, function
   ;; cells, and value cells that are needed for the execution of the
   ;; function.
   (%environment :initarg :environment :reader environment)))
