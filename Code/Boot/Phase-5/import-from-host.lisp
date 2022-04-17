(cl:in-package #:sicl-boot-phase-5)

(defun import-from-host (e5)
  (import-functions-from-host
   '(;; MISMATCH is used by the string comparison functions.  And
     ;; STRING= is called at compile time by the LOOP macro to
     ;; determine which LOOP keyword was given.
     mismatch)
   e5))
