(cl:in-package #:sicl-clos)

(defclass sequence (t)
  ()
  (:metaclass built-in-class))

(defclass list (sequence) ()
  (:metaclass built-in-class))

(defclass hash-table () ())

(defclass readtable () ())

(defclass pathname () ())

(defclass package () ())

(defclass stream () ())

(defclass random-state () ())

(defclass structure-object (standard-object) ())

(defclass cons (t) ()
  (:metaclass built-in-class))

(defclass character (t) ()
  (:metaclass built-in-class))

(defclass symbol () ())

(defclass null (symbol list) ())

;;; This one is for Predicament.
(defclass string () ())
