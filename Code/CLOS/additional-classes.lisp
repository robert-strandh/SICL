(in-package #:sicl-clos)

(define-built-in-class sequence (t)
  ())

(define-built-in-class list (sequence)
  ())

(define-built-in-class symbol ()
  ((%name :initarg :name :reader symbol-name)
   (%package :initarg :package :reader symbol-package)))

(define-built-in-class null (symbol list)
  ())


