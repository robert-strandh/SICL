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

(defclass number () ()
  (:metaclass built-in-class))

(defclass complex (number) ()
  (:metaclass built-in-class))
