;;;; Copyright (c) 2014
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the string module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file string.text for a description of the module.

(cl:in-package #:sicl-string)

(defgeneric report-string-condition (condition stream))

(define-condition bag-is-dotted-list
    (type-error cleavir-i18n:condition)
  ())

(define-condition bag-is-circular-list
    (type-error cleavir-i18n:condition)
  ()
  (:report report-string-condition))

(define-condition bag-contains-non-character
    (type-error cleavir-i18n:condition)
  ()
  (:report report-string-condition))

(define-condition invalid-bounding-indices
    (error cleavir-i18n:condition)
  ((%target :initarg :target :reader target)
   (%start :initarg start :reader start)
   (%end :initarg end :reader end)))
