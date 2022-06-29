(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-load-time
  :depends-on (#:sicl-arithmetic-base)
  :serial t
  :components
  ((:file "number-defclass")
   (:file "real-defclass")
   (:file "rational-defclass")
   (:file "integer-defclass")
   (:file "fixnum-defclass")
   (:file "bignum-defclass")
   (:file "ratio-defclass")
   (:file "float-defclass")
   (:file "floatp-defgeneric")
   (:file "single-float-defclass")
   (:file "double-float-defclass")
   (:file "complex-defclass")))
