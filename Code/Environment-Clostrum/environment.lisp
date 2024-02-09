(cl:in-package #:sicl-environment)

(defclass run-time-environment (clostrum-basic:run-time-environment)
  (;; This slot holds an EQ hash table, mapping symbols to
   ;; method-combination templates.
   (%method-combination-templates
    :initform (make-hash-table :test #'eq)
    :accessor method-combination-templates)))

(defmethod find-method-combination-template
    (symbol (env run-time-environment))
  (gethash symbol (method-combination-templates env)))

(defmethod (setf find-method-combination-template)
    (new-template symbol (env run-time-environment))
  (setf (gethash symbol (method-combination-templates env)) new-template)
  new-template)
