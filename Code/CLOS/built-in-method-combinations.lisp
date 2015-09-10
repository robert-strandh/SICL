(cl:in-package #:sicl-clos)

(define-method-combination progn :identity-with-one-argument t)

(define-method-combination and :identity-with-one-argument t)

(define-method-combination or :identity-with-one-argument t)

(define-method-combination append :identity-with-one-argument t)

(define-method-combination nconc :identity-with-one-argument t)

(define-method-combination list :identity-with-one-argument nil)

(define-method-combination max :identity-with-one-argument t)

(define-method-combination min :identity-with-one-argument t)

(define-method-combination + :identity-with-one-argument t)
