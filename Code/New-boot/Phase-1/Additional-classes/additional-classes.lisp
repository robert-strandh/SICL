(cl:in-package #:sicl-clos)

(defclass sequence (t)
  ()
  (:metaclass built-in-class))

(defclass list (sequence) ()
  (:metaclass built-in-class))

(defclass hash-table () ())

(defclass readtable () ())

(defclass pathname () ())

(defclass stream () ())

(defclass random-state () ())

(defclass structure-object (standard-object) ())

(defclass number (t) ()
  (:metaclass built-in-class))

(defclass fixnum (number) ()
  (:metaclass built-in-class))

(defclass cons (t) ()
  (:metaclass built-in-class))

(defclass character (t) ()
  (:metaclass built-in-class))

(defclass complex (number) ()
  (:metaclass built-in-class))
