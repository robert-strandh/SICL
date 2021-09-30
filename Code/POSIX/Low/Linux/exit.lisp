(cl:in-package #:sicl-posix-low)

(defparameter *exit-commands*
  (append
   ;; We are given the exit status as a fixnum in the first argument,
   ;; so we start by shifting it one position to get a native integer.
   (unbox-fixnum *rdi*)
   (syscall 60)))
