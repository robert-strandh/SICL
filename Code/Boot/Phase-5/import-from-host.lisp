(cl:in-package #:sicl-boot-phase-5)

(defun import-from-host (e5)
  (import-functions-from-host
   '(;; MISMATCH is used by the string comparison functions.  And
     ;; STRING= is called at compile time by the LOOP macro to
     ;; determine which LOOP keyword was given.
     mismatch
     ;; NREVERSE and POSITION-IF-NOT are called by FORMAT to parse
     ;; arguments.  And the compiler macro of FORMAT is called at
     ;; compile time, so these functions are needed at compile time.
     nreverse position-if-not)
   e5))
