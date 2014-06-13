(in-package #:sicl-compiler)

(defgeneric process-compound-form (head form environment))

(defun process-top-level-form (form)
  (setf form (sicl-env:fully-expand-form form nil))
  (if (and (consp form) (not (eq (car form) 'quote)))
      (process-compound-form (car form) form nil)
      nil))

(defun compile-file (input-file &key
				(output-file nil output-file-p)
				(verbose *compile-verbose*)
				(print *compile-print*)
				(external-format :default))
  
  (declare (ignore output-file output-file-p verbose print))
  (with-open-file (stream input-file
			  :direction :input
			  :external-format external-format)
    (let* ((*compile-file-pathname* (merge-pathnames input-file))
	   (*compile-file-truename* (truename *compile-file-pathname*))
	   (*package* *package*)
	   (*readtable* *readtable*)
	   (sicl-compiler-phase-1:*compile-file* t))
      (sicl-ast:make-progn-ast 
       (loop with eof-value = (list nil)
	     for form = (sicl-reader:read stream nil eof-value)
	     until (eq form eof-value)
	     for result = (sicl-compiler-phase-1:convert-initial form)
	     unless (null result)
	       collect result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on PROCESS-COMPOUND-FORM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process PROGN.
;;;
;;; The subforms of a PROGN form are considered to be top-level forms
;;; so they should be processed just like the form itself.

(defmethod process-compound-form ((head (eql 'progn)) form environment)
  (loop for subform in (rest form)
	do (process-compound-form subform environment)))

