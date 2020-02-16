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

(defclass always-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause always-clause))
  `((nil always/never t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser always-clause-parser
  (consecutive (lambda (always form)
                 (declare (ignore always))
                 (make-instance 'always-clause
                   :form form))
               (keyword-parser 'always)
               'anything-parser))

(add-clause-parser 'always-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause always-clause) end-tag)
  (declare (ignore end-tag))
  `(unless ,(form clause)
     (return-from ,*loop-name* nil)))
