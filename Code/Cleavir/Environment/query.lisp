(cl:in-package #:cleavir-environment)

(defgeneric variable-info (environment symbol))

(defclass lexical-variable-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg identity :reader identity)
   (%type :initarg :type :reader type)
   (%ignore :initarg :ignore :reader ignore)
   (%dynamic-extent :initarg :dynamic-extent :reader dynamic-extent)))

(defclass special-variable-info ()
  ((%name :initarg :name :reader name)
   (%type :initarg :type :reader type)
   (%ignore :initarg :ignore :reader ignore)))

(defclass constant-variable-info ()
  ((%name :initarg :name :reader name)
   (%value :initarg :value :reader value)))

(defclass symbol-macro-info ()
  ((%name :initarg :name :reader name)  
   (%type :initarg :type :reader type)
   (%expansion :initarg :expansion :reader expansion)))

(defgeneric function-info (environment function-name))

(defclass local-function-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg identity :reader identity)
   (%type :initarg :type :reader type)
   (%inline :initarg :inline :reader inline)
   (%ignore :initarg :ignore :reader ignore)
   (%dynamic-extent :initarg :dynamic-extent :reader dynamic-extent)))
  
(defclass global-function-info ()
  ((%name :initarg :name :reader name)
   (%type :initarg :type :reader type)
   (%inline :initarg :inline :reader inline)
   (%compiler-macro :initarg :compiler-macro :reader compiler-macro)
   (%ignore :initarg :ignore :reader ignore)
   (%dynamic-extent :initarg :dynamic-extent :reader dynamic-extent)))
  
(defclass local-macro-info ()
  ((%name :initarg :name :reader name)
   (%expander :initarg :expander :reader expander)))

(defclass global-macro-info ()
  ((%name :initarg :name :reader name)
   (%expander :initarg :expander :reader expander)
   (%compiler-macro :initarg :compiler-macro :reader compiler-macro)))

(defgeneric block-info (environment symbol))

(defclass block-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defgeneric tag-info (environment tag))

(defclass tag-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defgeneric optimize-info (environment))

(defclass optimize-info ()
  ((%speed :initform 3 :initarg :speed :reader speed)
   (%debug :initform 3 :initarg :debug :reader debug)
   (%compilation-speed :initform 3
		       :initarg :compilation-speed :reader compilation-speed)
   (%space :initform 3 :initarg :space :reader space)
   (%safety :initform 3 :initarg :safety :reader safety)))

(defgeneric type-expand (environment type))
