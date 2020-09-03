(cl:in-package #:sicl-environment)

(defclass base-run-time-environment
    (clostrum/virtual:virtual-run-time-environment)
  (;; This slot holds an EQ hash table, mapping symbols to
   ;; method-combination templates.
   (%method-combination-templates :initform (make-hash-table :test #'eq)
                                  :accessor method-combination-templates)))

(defclass run-time-environment
    (base-run-time-environment)
  ((%client :initarg :client :reader client)))

(defclass evaluation-environment
    (base-run-time-environment
     clostrum:evaluation-environment-mixin)
  ())

(defclass compilation-environment
    (clostrum/virtual:virtual-compilation-environment)
  ())

(defmethod client ((environment compilation-environment))
  (client (clostrum:parent environment)))

(defmethod client ((environment evaluation-environment))
  (client (clostrum:parent environment)))
