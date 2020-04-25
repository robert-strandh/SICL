(in-package #:sicl-simple-environment)

(define-condition redefinition (program-error acclimation:condition)
  ((%name :initarg :name :reader name)
   (%newtype :initarg :newtype :reader newtype)
   (%oldtype :initarg :oldtype :reader oldtype)))

(define-condition constant-redefinition (program-error acclimation:condition)
  ((%name :initarg :name :reader name)
   (%old :initarg :old :reader old)
   (%new :initarg :new :reader new)))

(define-condition attempt-to-proclaim-type (program-error acclimation:condition)
  ((%name :initarg :name :reader name)))

(define-condition attempt-to-proclaim-type-of-special-operator (attempt-to-proclaim-type)
  ((%name :initarg :name :reader name)))

(define-condition attempt-to-proclaim-type-of-macro (attempt-to-proclaim-type)
  ((%name :initarg :name :reader name)))

(define-condition attempt-to-proclaim-type-of-constant (attempt-to-proclaim-type)
  ((%name :initarg :name :reader name)))

(define-condition undefined-function (cl:undefined-function acclimation:condition)
  ((%environment :initarg :environment :reader environment)))
