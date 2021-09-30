(cl:in-package #:sicl-posix-low)

(defparameter *read-commands*
  (append
   ;; We are given the file descriptor as a fixnum in the first
   ;; argument, so we start by turning it into a native integer.
   (unbox-fixnum *rdi*)
   ;; Set RDI to a pointer to the first element of the buffer.
   (rack-from-standard-object *rsi* *rsi*)
   (first-vector-element-from-rack *rsi*)
   ;; We are given the count as a fixnum, so unbox it.
   (unbox-fixnum *rsi*)
   (syscall 0)
   (adjust-return-value)
   (return-to-caller)))
