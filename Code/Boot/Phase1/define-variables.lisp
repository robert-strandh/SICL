(cl:in-package #:sicl-boot-phase1)

(defvar *t*
  (find-class 't))

(defvar *standard-object*)

(defvar *funcallable-standard-object*)

(defvar *standard-class*
  (find-class 'standard-class))

(defvar *funcallable-standard-class*
  (find-class 'funcallable-standard-class))

(defvar *standard-reader-method*
  (find-class 'standard-reader-method))

(defvar *standard-writer-method*
  (find-class 'standard-writer-method))

(defvar *standard-direct-slot-definition*
  (find-class 'standard-direct-slot-definition))

(defvar *standard-effective-slot-definition*
  (find-class 'standard-effective-slot-definition))

(defvar *standard-reader-method*
  (find-class 'standard-reader-method*))

(defvar *standard-writer-method*
  (find-class 'standard-writer-method*))
