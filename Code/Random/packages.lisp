(cl:in-package #:common-lisp-user)

(defpackage #:sicl-random
  (:use :common-lisp)
  (:export
   #:random-state
   #:make-random-state
   #:random
   #:random-state-p
   #:*random-state*)
  (:shadow
   #:random-state
   #:make-random-state
   #:random
   #:random-state-p
   #:*random-state*))
