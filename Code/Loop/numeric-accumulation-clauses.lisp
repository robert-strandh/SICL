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
;;; Clause NUMERIC-ACCUMULATION-CLAUSE and subclasses.
;;;
;;; The non-terminal symbol used in the HyperSpec is
;;; NUMERIC-ACCUMULATION rather than NUMERIC-ACCUMULATION-CLAUSE as we
;;; call it here.

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform t :initarg :type-spec :accessor type-spec)))


