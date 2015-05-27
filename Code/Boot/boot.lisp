(cl:in-package #:sicl-boot)

(defclass boot ()
  ((%e1 :initarg :e1 :reader e1)
   (%e2 :initarg :e2 :reader e2)
   (%e3 :initarg :e3 :reader e3)
   (%c1 :initarg :c1 :reader c1)
   (%c2 :initarg :c2 :reader c2)))
