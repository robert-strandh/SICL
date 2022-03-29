(cl:in-package #:sicl-compiler)

(define-condition compile-time-warning (warning)
  ((%source-location
    :initform nil
    :initarg :source-location
    :reader source-location)))

(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition undefined-function (compile-time-warning name-mixin)
  ()
  (:report (lambda (condition stream)
             (let ((*package* (find-package "KEYWORD")))
               (format stream
                       "Undefined function named ~s"
                       (name condition))))))

(define-condition undefined-variable (compile-time-warning name-mixin)
  ()
  (:report (lambda (condition stream)
             (let ((*package* (find-package "KEYWORD")))
               (format stream
                       "Undefined variable named ~s"
                       (name condition))))))

(define-condition undefined-block (compile-time-warning name-mixin)
  ()
  (:report (lambda (condition stream)
             (let ((*package* (find-package "KEYWORD")))
               (format stream
                       "Undefined block named ~s"
                       (name condition))))))

(define-condition undefined-tagbody-tag (compile-time-warning name-mixin)
  ()
  (:report (lambda (condition stream)
             (let ((*package* (find-package "KEYWORD")))
               (format stream
                       "Undefined tagbody tag named ~s"
                       (name condition))))))
