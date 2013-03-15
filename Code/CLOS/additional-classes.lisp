(in-package #:sicl-clos)

(define-built-in-class sequence (t))

(define-built-in-class list (sequence))

(defclass symbol ()
  ((%name :initarg :name :reader symbol-name)
   (%package :initarg :package :reader symbol-package)
   (%plist :initform '() :accessor symbol-plist)))

(define-built-in-class null (symbol list))
