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

(defclass while-clause (clause)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser while-clause-parser
  (consecutive (lambda (while form)
		 (declare (ignore while))
		 (make-instance 'while-clause
		   :form form))
	       (keyword-parser 'while)
	       (singleton #'identity (constantly t))))

(add-clause-parser 'while-clause-parser)

(define-parser until-clause-parser
  (consecutive (lambda (until form)
		 (declare (ignore until))
		 (make-instance 'while-clause
		   :form `(not ,form)))
	       (keyword-parser 'until)
	       (singleton #'identity (constantly t))))
  
(add-clause-parser 'until-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-forms

(defmethod termination-forms ((clause while-clause) end-tag)
  `(unless ,(form clause)
     (go ,end-tag)))
