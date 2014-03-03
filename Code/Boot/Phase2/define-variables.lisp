(cl:in-package #:sicl-boot-phase2)

(defvar *t*
  (find-class 't))

(defvar *standard-reader-method*
  (find-class 'sicl-boot-phase1:standard-reader-method))

(defvar *standard-writer-method*
  (find-class 'sicl-boot-phase1:standard-writer-method))
