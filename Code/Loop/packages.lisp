;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
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

;;;; This file is part of the loop module of the SICL project.
;;;; See the file SICL.text for a description of the project. 

(cl:in-package #:common-lisp-user)

(defpackage #:sicl-loop
  (:use #:common-lisp
	#:sicl-additional-conditions)
  (:shadow
   ;; We use TYPE as an accessor for a TYPE-SPEC so we need to shadow
   ;; this name.
   #:type
   ;; We use CONDITION as an accessor for a conditional clause so we
   ;; need to shadow this name
   #:condition
   )
  (:export #:define-parser
	   #:clause
	   #:subclauses-mixin
	   #:var-and-type-spec-mixin
	   #:compound-forms-mixin
	   #:loop-return-clause-mixin))
