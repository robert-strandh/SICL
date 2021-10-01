(cl:in-package #:sicl-posix-low)

(defparameter *close-commands*
  (append
   ;; We are given the file descriptor as a fixnum in the first
   ;; argument, so we start by turning it into a native integer.
   (unbox-fixnum *rdi*)
   (syscall 3)
   (adjust-return-value)
   (return-to-caller)))
