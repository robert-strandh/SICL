(in-package #:sicl-compiler)

(defparameter *compile-time-too* nil)

(defgeneric process-compound-form (head form environment))

(defun process-top-level-form (form environment)
  (setf form (sicl-env:fully-expand-form form environment))
  (if (and (consp form) (not (eq (car form) 'quote)))
      (process-compound-form (car form) form environment)
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
;;; The subforms of a top-level PROGN form are considered to be
;;; top-level forms so they should be processed just like the form
;;; itself.

(defmethod process-compound-form ((head (eql 'progn)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (loop for subform in (rest form)
	do (process-top-level-form subform environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process LOCALLY.
;;;
;;; The subforms of a top-level LOCALLY form are considered to be
;;; top-level forms so they should be processed as top-level forms in
;;; an environment that has been augmented by the declarations.

(defmethod process-compound-form ((head (eql 'locally)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (multiple-value-bind (declarations forms)
      (sicl-code-utilities:separate-ordinary-body (rest form))
    (let ((new-env (sicl-env:augment-environment-with-declarations
		    environment declarations)))
      (loop for subform in forms
	    do (process-top-level-form subform new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process SYMBOL-MACROLET.
;;;
;;; The subforms of a top-level SYMBOL-MACROLET form are considered to
;;; be top-level forms so they should be processed as top-level forms
;;; in an environment that has been augmented by the macro definitions. 

(defmethod process-compound-form ((head (eql 'symbol-macrolet)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (definitions . forms) (rest form)
    ;; FIXME check that each definition is a proper list of 2 elements.
    (let ((new-env env))
      (loop for (name expansion) in definitions
	    do (setf new-env
		     (sicl-env:add-symbol-macro-entry new-env name expansion)))
      (loop for subform in forms
	    do (process-top-level-form subform new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process MACROLET.
;;;
;;; The subforms of a top-level MACROLET form are considered to be
;;; top-level forms so they should be processed as top-level forms in
;;; an environment that has been augmented by the macro definitions.
;;;
;;; FIXME: implement this method.  It is tricky because it can not be
;;; defined in the cross compiler (as far as I can tell). 
;;; Consider the following example of a top-level form:
;;;
;;; (macrolet ((m (x) `(+ ,x 2)))
;;;   (eval-when (:compile-toplevel)
;;;     (let ((y 10))
;;;       (print (m y)))))
;;;
;;; The evaluator that evaluates the LET form must be aware of the
;;; augmented environment introduced by the MACROLET.  However, when
;;; we are cross compiling, the evaluator is the host EVAL function,
;;; and the augmented environment belongs to the target.  
;;;
;;; However, the augmented part of the environment contains only
;;; MACROLETs, SYMBOL-MACROLETs, and declarations introduced by
;;; LOCALLY.  It might be possible to construct a new form for the
;;; host EVAL function to evaluate, namely the form surrounded by the
;;; MACROLETs, the SYMBOL-MACROLETs, and the declarations of the
;;; augmented part of the environment.  In the example above, one
;;; could construct:
;;;
;;; (macrolet ((m (x) `(+ ,x 2)))
;;;   (let ((y 10))
;;;     (print (m y))))
;;;
;;; This method is not QUITE correct though, because macros may
;;; contain side effects (they shouldn't, but they may) in which case
;;; those side effects would be evaluated multiple times. 
;;;
;;; It is probably simpler to assume that the cross compiler is never
;;; used on forms that contain MACROLET, SYMBOL-MACROLET or LOCALLY as
;;; top-level forms, and just signal an error for those cases.
;;;
;;; In the native compiler things are easy, of course.  Just compile
;;; the form in the augmented environment, and then execute the
;;; result.

(defmethod process-compound-form ((head (eql 'macrolet)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (declare (ignore environment))
  (error "MACROLET not implemented yet."))
