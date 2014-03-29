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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause LIST-ACCUMULATION-CLAUSE.
;;;
;;; The non-terminal symbol used in the HyperSpec is LIST-ACCUMULATION
;;; rather than LIST-ACCUMULATION-CLAUSE as we call it here.

(defclass list-accumulation-clause (accumulation-clause)
  ((%into-tail-var :initform nil :initarg :into-tail-var :accessor into-tail-var)))

(defclass collect-clause (list-accumulation-clause) ())
(defclass append-clause (list-accumulation-clause) ())
(defclass nconc-clause (list-accumulation-clause) ())

