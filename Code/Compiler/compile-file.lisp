(in-package #:sicl-compiler)

;;;; This file contains the logic that is specific to COMPILE-FILE and
;;;; CROSS-COMPILE-FILE, i.e., the foreign file compiler.
;;;;
;;;; The cross compiler has some restrictions related to the fact that
;;;; top-level forms do not necessarily appear in a null lexical
;;;; environment as discussed in CLHS 3.2.3.1.  When an EVAL-WHEN with
;;;; a :COMPILE-TOPLEVEL appears in a non-null lexical environment, we
;;;; can not take the augmented part of the environment into account.
;;;; Consider the following example of a top-level form:
;;;;
;;;; (macrolet ((m (x) `(+ ,x 2)))
;;;;   (eval-when (:compile-toplevel)
;;;;     (let ((y 10))
;;;;       (print (m y)))))
;;;;
;;;; The evaluator that evaluates the LET form must be aware of the
;;;; augmented environment introduced by the MACROLET.  However, when
;;;; we are cross compiling, the evaluator is the host EVAL function,
;;;; but the augmented environment belongs to the target.  
;;;;
;;;; Though, since the augmented part of the environment contains only
;;;; MACROLETs, SYMBOL-MACROLETs, and declarations introduced by
;;;; LOCALLY.  It might be possible to construct a new form for the
;;;; host EVAL function to evaluate, namely the form surrounded by the
;;;; MACROLETs, the SYMBOL-MACROLETs, and the declarations of the
;;;; augmented part of the environment.  In the example above, one
;;;; could construct:
;;;;
;;;; (macrolet ((m (x) `(+ ,x 2)))
;;;;   (let ((y 10))
;;;;     (print (m y))))
;;;;
;;;; This method is not QUITE correct though, because macros may
;;;; contain side effects (they shouldn't, but they may) in which case
;;;; those side effects would be evaluated multiple times. 
;;;;
;;;; Instead of attempting to implement the method mentioned above, we
;;;; simply assume that when we are cross compiling, and an EVAL-WHEN
;;;; with a :COMPILE-TOPLEVEL situation appears, then it is in a null
;;;; lexical environment, and we signal an error if this is not the
;;;; case.  This restriction is not terribly severe, because we can
;;;; control what files are cross compiled and how they look. 
;;;;
;;;; In the native compiler things are easy, of course.  Just compile
;;;; the form in the augmented environment, and then execute the
;;;; result.

;;; The value of this variable is the PROCESSING MODE for top-level
;;; forms as indicated in CLHS 3.2.3.1.  
(defparameter *compile-time-too* nil)

;;; This variable is set to true when the file compiler is used as a
;;; cross compiler, and to false when it is used as a native compiler.
(defvar *cross-compiling-p*)

;;; This variable holds a list of the top-level ASTs.  While these
;;; ASTs are accumulated, the list is in reverse order with respect to
;;; occurrence in the file.
(defvar *top-level-asts*)

(defgeneric process-compound-form (head form environment))

(defun process-top-level-form (form environment)
  (setf form (sicl-env:fully-expand-form form environment))
  (if (and (consp form) (not (eq (car form) 'quote)))
      (process-compound-form (car form) form environment)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on PROCESS-COMPOUND-FORM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method.  Convert to AST and accumulate.

(defmethod process-compound-form (head form environment)
  (declare (ignore head))
  (push (sicl-generate-ast:generate-ast form environment)
	*top-level-asts*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This :BEFORE method handles compile-time evaluation.

;; This function takes care of testing the conditions for evaluation
;; in the cross compiler.
(defun maybe-evaluate (form environment)
  (if *cross-compiling-p*
      (if (null environment)
	  (eval form)
	  (error "Attempt to evaluate in a ~@
                             non-null lexical environment."))
      ;; FIXME: Once the native EVAL is written, use it here.
      nil))

(defmethod process-compound-form :before (head form environment)
  (declare (ignore head))
  (when *compile-time-too*
    (maybe-evaluate form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process PROGN.
;;;
;;; The subforms of a top-level PROGN form are considered to be
;;; top-level forms so they should be processed just like the form
;;; itself.

(defmethod process-compound-form ((head (eql 'progn)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
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
  (cleavir-code-utilities:check-form-proper-list form)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (rest form))
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
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (definitions . forms) (rest form)
    ;; FIXME check that each definition is a proper list of 2 elements.
    (let ((new-env environment))
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
;;; FIXME: implement this method.

(defmethod process-compound-form ((head (eql 'macrolet)) form environment)
  (declare (ignore environment))
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (error "MACROLET not implemented yet."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process EVAL-WHEN.

(defmethod process-compound-form ((head (eql 'eval-when)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (situations . forms) (rest form)
    (unless (cleavir-code-utilities:proper-list-p situations)
      (error 'situations-must-be-proper-list
	     :expr situations))
    ;; Check each situation
    (loop for situation in situations
	  do (unless (and (symbolp situation)
			  (member situation
				  '(:compile-toplevel :load-toplevel :execute
				    compile load eval)))
	       ;; FIXME: perhaps we should warn about the deprecated
	       ;; situations
	       (error 'invalid-eval-when-situation
		      :expr situation)))
    (let ((ct (intersection '(:compile-toplevel compile) situations))
	  (lt (intersection '(:load-toplevel load) situations))
	  (e (intersection '(:execute eval) situations)))
      ;; The following COND form exactly mimics the table in
      ;; CLHS 3.2.3.1.
      (cond ((or (and ct lt)
		 (and (not ct) lt e *compile-time-too*))
	     (let ((*compile-time-too* t))
	       (loop for subform in forms
		     do (process-top-level-form subform environment))))
	    ((or (and (not ct) lt e (not *compile-time-too*))
		 (and (not ct) lt (not e)))
	     (let ((*compile-time-too* nil))
	       (loop for subform in forms
		     do (process-top-level-form subform environment))))
	    ((or (and ct (not lt))
		 (and (not ct) (not lt) e *compile-time-too*))
	     (loop for subform in forms
		   do (maybe-evaluate subform environment)))
	    (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function COMPILE-FILE.

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
	   (*cross-compiling-p* nil)
	   (*top-level-asts* '()))
      (loop with eof-value = (list nil)
	    for form = (sicl-reader:read stream nil eof-value)
	    until (eq form eof-value)
	    do (process-top-level-form form nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CROSS-COMPILE-FILE.

(defun cross-compile-file (input-file &key
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
	   (*cross-compiling-p* t)
	   (*top-level-asts* '()))
      (loop with eof-value = (list nil)
	    for form = (sicl-reader:read stream nil eof-value)
	    until (eq form eof-value)
	    do (process-top-level-form form nil)))))
