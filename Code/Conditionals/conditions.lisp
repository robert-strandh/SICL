(cl:in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

(define-condition malformed-body
    (program-error acclimation:condition)
  ((%body :initarg :body :reader body))
  (:report (lambda (condition stream)
             (format stream
                     "Expected a proper list of forms,~@
                      but the following was given instead:~@
                      ~s"
                     (body condition)))))
     
(define-condition malformed-cond-clauses
    (program-error acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses))
  (:report (lambda (condition stream)
             (format stream
                     "Expected a proper list of cond clauses,~@
                      but the following was given instead:~@
                      ~s"
                     (clauses condition)))))
     
(define-condition malformed-cond-clause
    (program-error acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition malformed-case-clauses
    (program-error acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-case-clause
    (program-error acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition otherwise-clause-not-last
    (program-error acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-keys
    (program-error acclimation:condition)
  ((%keys :initarg :keys :reader keys)))
     
(define-condition malformed-typecase-clauses
    (program-error acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-typecase-clause
    (program-error acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition ccase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ccase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition etypecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in etypecasecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition ctypecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ctypecasecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))
