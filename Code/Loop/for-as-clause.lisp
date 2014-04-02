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
;;; Manage a list of FOR-AS subclause parsers. 

(defparameter *for-as-subclause-parsers* '())

(defun add-for-as-subclause-parser (parser)
  (push parser *for-as-subclause-parsers*))

;;; A parser that tries every parser in *FOR-AS-SUBCLAUSE-PARSERS* until one
;;; succeeds.

(defun for-as-subclause-parsers (tokens)
  (loop for parser in *for-as-subclause-parsers*
	do (multiple-value-bind (successp result rest)
	       (funcall parser tokens)
	     (when successp
	       (return (values t result rest))))
	finally (return (values nil nil tokens))))
