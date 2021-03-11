(cl:in-package #:sicl-register-allocation)

(defclass locations ()
  ((%lexical-location
    :initarg :lexical-location
    :reader lexical-location)
   (%register
    :initform nil
    :initarg :register
    :accessor register)
   (%stack-location
    :initform nil
    :initarg stack-location
    :accessor stack-location)))

(defclass assignment ()
  ((%stack-map :initarg :stack-map :reader stack-map)
   (%locations
    :initform '()
    :initarg :locations
    :accessor locations)))
