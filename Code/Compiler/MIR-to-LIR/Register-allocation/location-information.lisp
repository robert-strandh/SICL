(cl:in-package #:sicl-register-allocation)

(defclass location-information ()
  (;; This slot contains a hash table that maps every lexical
   ;; location to a unique small non-negative integer.  The table is
   ;; shared between all instances of this class.
   (unique-number :initarg :unique-number :reader unique-number)
   ;; This slot contains a bitmap indexed by the unique integer
   ;; associated with a lexical location.  It contains a 1 if the
   ;; location is live and a 0 otherwise.
   (liveness :initarg :liveness :reader liveness)))
    
