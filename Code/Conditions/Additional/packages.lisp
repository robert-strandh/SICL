(cl:in-package #:common-lisp-user)

(defpackage #:sicl-additional-conditions
  (:use #:common-lisp #:sicl-additional-types)
  (:export
   #:sicl-condition
   #:sicl-warning
   #:sicl-style-warning
   #:sicl-error
   #:sicl-type-error
   #:sicl-cell-error
   #:invalid-bounding-indexes
   #:sicl-program-error
   #:sicl-program-warning
   #:sicl-program-style-warning)
  (:shadow #:sequence))
