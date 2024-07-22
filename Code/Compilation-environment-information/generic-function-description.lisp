(cl:in-package #:sicl-lexical-environment)

(defclass generic-function-description (trucler:generic-function-description)
  ((%generic-function-class-name
    :initarg :generic-function-class-name
    :reader generic-function-class-name)
   (%method-class-name
    :initarg :method-class-name
    :reader method-class-name)
   (%method-combination-name
    :initarg :method-combination-name
    :reader method-combination-name)
   (%method-combination-arguments
    :initform '()
    :initarg :method-combination-arguments
    :reader method-combination-arguments)))
