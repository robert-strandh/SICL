(cl:in-package #:sicl-boot-phase-5)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  (;; This slot contains the slot accessor SICL-CLOS:ENVIRONMENT
   ;; which, when given an ersatz instance of the SICL class
   ;; FUNCALLABLE-STANDARD-OBJECT, returns the static environment of
   ;; that function.
   (%static-environment-function
    :initarg :static-environment-function
    :accessor static-environment-function)))
