(cl:in-package #:sicl-environment)

(defclass base-run-time-environment
    (clostrum/virtual:virtual-run-time-environment)
  (;; This slot holds an EQ hash table, mapping symbols to
   ;; method-combination templates.
   (%method-combination-templates :initform (make-hash-table :test #'eq)
                                  :accessor method-combination-templates)))

(defmethod find-method-combination-template
    (symbol (env base-run-time-environment))
  (gethash symbol (method-combination-templates env)))

(defmethod (setf find-method-combination-template)
    (new-template symbol (env base-run-time-environment))
  (setf (gethash symbol (method-combination-templates env)) new-template)
  new-template)

(defclass run-time-environment
    (base-run-time-environment)
  ((%client :initarg :client :reader client)
   (%traced-functions :initform (make-hash-table :test #'equal)
                      :reader traced-functions)))

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

(defmethod client ((environment trucler-reference:environment))
  #+(or)(warn "Function CLIENT called with a lexical environment")
  (let* ((client (make-instance 'trucler-reference:client))
         (global-environment (trucler:global-environment client environment)))
    (client global-environment)))

(defmethod fdefinition (client (environment trucler-reference:environment) name)
  #+(or)(warn "Function FDEFINITION called with a lexical environment and name: ~s" name)
  (let ((global-environment (trucler:global-environment client environment)))
    (fdefinition client global-environment name)))
