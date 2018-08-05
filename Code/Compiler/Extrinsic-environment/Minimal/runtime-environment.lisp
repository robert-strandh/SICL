(cl:in-package #:sicl-minimal-extrinsic-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dynamic run-time environment.
;;;
;;; The dynamic run-time environment is a list of instances of the
;;; class DYNAMIC-ENVIRONMENT-ENTRY.
;;;
;;; In a native backend, the dynamic run-time environment would be
;;; passed in a register to each function, which is kind of like a
;;; hidden argument.  But in the extrinsic environment we do not want
;;; to add arguments to each function, because we want the
;;; function-call protocol to be compatible with that of the host, so
;;; that we can call host functions that ignore the existence of the
;;; target dynamic run-time environment.
;;;
;;; We solve this problem as follows: we define a host variable named
;;; *DYNAMIC-ENVIRONMENT* that holds the dynamic run-time environment
;;; of the target AROUND TARGET FUNCTION CALLS ONLY.  This host
;;; variable is SET (not bound) right before a function is called, and
;;; a lexical variable in the callee is bound to its value immediately
;;; upon function entry.  In a multi-threaded environment, this host
;;; variable must be BOUND for the duration of the thread so that each
;;; thread sees its own binding.

(defparameter *dynamic-environment* '())

(defclass dynamic-environment-entry () ())

(defclass variable-binding (dynamic-environment-entry)
  ((%symbol :initarg :symbol :reader symbol)
   (%value :initarg :value :accessor value)))

(defclass unwind-protect (dynamic-environment-entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defclass catch-tag (dynamic-environment-entry)
  ((%value :initarg :value :reader value)
   ;; The function in this slot takes a single argument which must be
   ;; a list of values to return from the CATCH.  When invoked, this
   ;; function executes the host form (RETURN-FROM <block-name> (APPLY
   ;; #'VALUES ARG)).
   (%function :initarg :function :reader function)))

;;; SUFFIX is a suffix of the current dynamic environment.  This
;;; function executes any UNWIND-PROTECT entries starting with the
;;; dynamic run-time environment provided by the caller in the
;;; variable *DYNAMIC-ENVIRONMENT* and ending with the last entry
;;; before SUFFIX is reached.
;;;
;;; The value of the variable *DYNAMIC-ENVIRONMENT* is only valid
;;; around function calls.  Each function has its own local variable
;;; containing the dynamic run-time environment as it needs it to be.
;;; That includes the function in which the UNWIND-PROTECT was
;;; executed.  Therefore the thunk stored in the UNWIND-PROTECT entry
;;; will be executed in the dynamic run-time environment of the
;;; function in which the UNWIND-PROTECT form was evaluated, which is
;;; the way it is supposed to be according to the HyperSpec.
(defun unwind (suffix)
  (loop for env = *dynamic-environment* then (cdr env)
	until (eq suffix env)
	;; If SUFFIX is not the same as ENV, then there is at least
	;; one entry between the two.
	do (let ((entry (first env)))
	     (when (typep entry 'unwind-protect)
	       (funcall (thunk entry))))))

;;; VALUES is a list of values that should be returned by the CATCH.
(defun throw (tag values)
  ;; Find the entry with the corresponding CATCH tag.  Right now we
  ;; signal an error in the host if the entry is not found.  Later, we
  ;; should signal an entry in the target environment instead.
  (let ((suffix (loop for suffix on *dynamic-environment*
		      for entry = (car suffix)
		      when (and (typep entry 'catch-tag)
				(eq (value entry) tag))
			return suffix
		      finally (error "no such tag ~s" tag))))
    (let ((function (function (car suffix))))
      (unwind (cdr suffix))
      (funcall function values))))

;;; A target CATCH is implemented like this:
;;;
;;; (CL:CATCH <tag> <form>*) =>
;;; (CL:BLOCK <symbol>
;;;   (CATCH <tag>
;;;     (CL:LAMBDA (VALUES)
;;;       (CL:RETURN-FROM <symbol>
;;;         (CL:APPLY #'CL:VALUES VALUES)))
;;;     (CL:LAMBDA ()
;;;       <form>*)))
;;;
;;; where CATCH is this function:
(defun catch (tag throw-fun body-fun)
  (let* ((entry (make-instance 'catch-tag
		  :value tag
		  :function throw-fun))
	 (*dynamic-environment* (cons entry *dynamic-environment*)))
    (funcall body-fun)))

;;; A target UNWIND-PROTECT is implemented like this:
;;;
;;; (CL:UNWIND-PROTECT <protected-form> <cleanup-form>*) =>
;;; (UNWIND-PROTECT
;;;   (CL:LAMBDA () <protected-form>)
;;;   (CL:LAMBDA () <cleanup-form>*))
;;;
;;; Where UNWIND-PROTECT is this function:
(defun unwind-protect (protect-fun cleanup-fun)
  (let* ((entry (make-instance 'unwind-protect
		  :thunk cleanup-fun))
	 (*dynamic-environment* (cons entry *dynamic-environment*)))
    (funcall protect-fun)))
