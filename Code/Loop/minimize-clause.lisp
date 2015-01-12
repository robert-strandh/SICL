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

(defclass minimize-clause (max/min-accumulation-clause) ())

(defclass minimize-it-clause (minimize-clause it-mixin)
  ())

(defclass minimize-form-clause (minimize-clause form-mixin)
  ())

(defclass minimize-it-into-clause (into-mixin minimize-clause it-mixin)
  ())

(defclass minimize-form-into-clause (into-mixin minimize-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-it-into-clause-parser
  (consecutive (lambda (minimize it into var type-spec)
		 (declare (ignore minimize it into))
		 (make-instance 'minimize-it-into-clause
		   :into-var var
		   :type-spec type-spec))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       (keyword-parser 'it)
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))
	       'optional-type-spec-parser))

(define-parser minimize-it-clause-parser
  (consecutive (lambda (minimize it type-spec)
		 (declare (ignore minimize it))
		 (make-instance 'minimize-it-clause
		   :type-spec type-spec))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       (keyword-parser 'it)
	       'optional-type-spec-parser))

(define-parser minimize-form-into-clause-parser
  (consecutive (lambda (minimize form into var type-spec)
		 (declare (ignore minimize into))
		 (make-instance 'minimize-form-into-clause
		   :form form
		   :into-var var
		   :type-spec type-spec))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       'anything-parser
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))
	       'optional-type-spec-parser))

(define-parser minimize-form-clause-parser
  (consecutive (lambda (minimize form type-spec)
		 (declare (ignore minimize))
		 (make-instance 'minimize-form-clause
		   :form form
		   :type-spec))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       'anything-parser
	       'optional-type-spec-parser))

(define-parser minimize-clause-parser
  (alternative 'minimize-it-into-clause-parser
	       'minimize-it-clause-parser
	       'minimize-form-into-clause-parser
	       'minimize-form-clause-parser))

(add-clause-parser 'minimize-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause minimize-form-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,*accumulation-variable*
	 (min ,*accumulation-variable* ,(form clause))))

(defmethod body-form ((clause minimize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,(into-var clause)
	 (min ,(into-var clause) ,(form clause))))

(defmethod body-form ((clause minimize-it-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,*accumulation-variable*
	 (min ,*accumulation-variable* ,*it-var*)))

(defmethod body-form ((clause minimize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,(into-var clause)
	 (min ,(into-var clause) ,*it-var*)))
