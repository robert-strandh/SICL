(cl:in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition malformed-body
    (program-error name-mixin acclimation:condition)
  ((%body :initarg :body :reader body)))
     
(define-condition malformed-cond-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-cond-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition malformed-case-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-case-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition otherwise-clause-not-last
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-keys
    (program-error name-mixin acclimation:condition)
  ((%keys :initarg :keys :reader keys)))
     
(define-condition malformed-typecase-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-typecase-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition ccase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition etypecase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition ctypecase-type-error
    (type-error name-mixin acclimation:condition)
  ())
