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

(cl:in-package #:common-lisp-user)

(defpackage #:sicl-string
  (:use #:common-lisp)
  (:export
   #:string
   #:make-string
   #:base-string
   #:simple-string
   #:simple-base-string
   #:stringp
   #:simple-string-p
   #:char
   #:schar
   #:string-upcase
   #:string-downcase
   #:string-capitalize
   #:nstring-upcase
   #:nstring-downcase
   #:nstring-capitalize
   #:string-trim
   #:string-left-trim
   #:string-right-trim
   #:string=
   #:string/=
   #:string<
   #:string>
   #:string<=
   #:string>=
   #:string-equal
   #:string-not-equal
   #:string-lessp
   #:string-greaterp
   #:string-not-greaterp
   #:string-not-lessp))
