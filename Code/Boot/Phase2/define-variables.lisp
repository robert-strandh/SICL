(cl:in-package #:sicl-boot-phase2)

(define-symbol-macro *t*
  (find-target-class 't))

(defvar *standard-reader-method*
  (find-bridge-class 'standard-reader-method))

(defvar *standard-writer-method*
  (find-bridge-class 'standard-writer-method))

(define-symbol-macro *standard-object*
    (find-target-class 'cl:standard-object))

(define-symbol-macro *funcallable-standard-object*
    (find-target-class 'funcallable-standard-object))

(defvar *standard-direct-slot-definition*
  (find-bridge-class 'standard-direct-slot-definition))

(defvar *standard-effective-slot-definition*
  (find-bridge-class 'standard-effective-slot-definition))
