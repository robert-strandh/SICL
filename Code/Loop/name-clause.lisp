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

(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

(defclass name-clause (clause)
  ((%name :initarg :name :reader name)))
