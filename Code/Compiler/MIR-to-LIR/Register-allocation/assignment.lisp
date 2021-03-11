(cl:in-package #:sicl-register-allocation)

(defclass locations ()
  ((%lexical-location
    :initarg :lexical-location
    :reader lexical-location)
   (%register
    :initform nil
    :initarg :register
    :accessor register)
   (%stack-slot
    :initform nil
    :initarg stack-slot
    :accessor stack-slot)))

(defclass assignment ()
  ((%stack-map :initarg :stack-map :reader stack-map)
   (%locations
    :initform '()
    :initarg :locations
    :accessor locations)))
