(cl:in-package #:sicl-random)

(defgeneric random-bits (random-state))

(defclass random-state ()
  ((%random-bits :initform (error "Amount of random bits must be specified.")
                 :reader random-bits
                 :documentation
                 "The amount of bits in the number returned by READ-RANDOM-STATE.")))
