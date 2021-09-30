(cl:in-package #:sicl-random)

(defgeneric seed-random-state (state seed)
  (:documentation "Initializes and sets the initial seed of STATE."))
(defgeneric read-random-state (state)
  (:documentation "Returns the next random number from STATE and prepares the next one."))
(defgeneric copy-random-state (state)
  (:documentation "Returns a new object which is a copy of STATE."))
