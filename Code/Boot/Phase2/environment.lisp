(cl:in-package #:sicl-boot-phase2)

(defclass compilation-environment (sicl-extrinsic-environment:environment)
  ())

(defun message (message-text)
  (format *trace-output* message-text))

(defmethod initialize-instance :around
    ((environment compilation-environment) &key)
  (message "Initializing phase 2 compilation environment~%")
  (call-next-method)
  (message "Finished initializing phase 2 compilation environment~%"))

(defclass environment (sicl-extrinsic-environment:environment)
  ((%phase1-environment
    :initarg :phase1-environment
    :initform (make-instance 'sicl-boot-phase1:environment)
    :reader phase1-environment)
   (%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'compilation-environment)
    :reader compilation-environment)))

;;; Before we can start creating bridge generic functions and bridge
;;; classes in the phase 2 run-time environment, we need to define
;;; some methods on host generic functions, specialized to classes
;;; that were created in phase 1.  The methods are auxiliary methods
;;; on the generic functions INITIALIZE-INSTANCE,
;;; REINITIALIZE-INSTANCE, and SHARED-INITIALIZE, and the purpose is
;;; to implement what the AMOP requires for classed, generic
;;; functions, and slot definitions.  We basically want to evaluate
;;; host DEFMETHOD forms that are loaded from files, except that the
;;; names of the specializer classes are wrong for the host
;;; environment.  When we created those specializer classes in phase
;;; 1, we gave them GENSYMed names so that they would not clash with
;;; existing host classes.
;;;
;;; We solve this problem by defining the macro DEFMETHOD in the phase
;;; 2 compilation environment to call the host EVAL with a slightly
;;; modified DEFMETHOD form.  It is modified so that the names of the
;;; specializer classes in the required parameters are those GENSYMed
;;; names that the host will recognize.
(defun define-defmethod (compilation-environment run-time-environment)
  (setf (sicl-genv:macro-function 'defmethod compilation-environment)
	(lambda (form environment)
	  (declare (ignore environment))
	  (let* ((name (second form))
		 (rest (rest (rest form)))
		 (lambda-list-position (position-if #'listp rest))
		 (lambda-list (nth lambda-list-position rest))
		 (qualifiers (subseq rest 0 (1- lambda-list-position)))
		 (body (subseq rest lambda-list-position))
		 (required-end-position
		   (position-if (lambda (x) (member x lambda-list-keywords))
				lambda-list))
		 (required (if (null required-end-position)
			       lambda-list
			       (subseq lambda-list 0 required-end-position)))
		 (remaining (if (null required-end-position)
				'()
				(subseq lambda-list required-end-position)))
		 (new-required
		   (loop for req in required
			 collect
			 (if (symbolp req)
			     req
			     (list (first req)
				   (class-name
				    (sicl-genv:find-class
				     (second req)
				     run-time-environment))))))
		 (new-args (append new-required remaining)))
	    `(eval '(defmethod ,name ,@qualifiers ,@new-args ,@body))))))

(defmethod initialize-instance :after ((environment environment) &key)
  (define-defmethod
      (compilation-environment environment)
      (phase1-environment environment))
  (fill-environment environment))
