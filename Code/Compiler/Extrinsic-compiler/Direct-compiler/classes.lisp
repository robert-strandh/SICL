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
