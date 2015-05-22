(cl:in-package #:sicl-boot-phase2)

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'sicl-extrinsic-environment:environment))
   (%phase1-environment
    :initarg :phase1-environment
    :initform (make-instance 'sicl-boot-phase1:environment)
    :reader phase1-environment)))

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
  (fill-environment environment))
