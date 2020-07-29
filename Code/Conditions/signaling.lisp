(in-package #:sicl-conditions)

;;; The stack of active restarts is a list of restarts ordered from
;;; most recent to least recent.

(defparameter *restart-stack* '())

;;; FIXME: do more syntax checks.
(defmacro restart-bind ((&rest bindings) &body body)
  (flet ((make-restart (binding)
	   (destructuring-bind (name function
				&rest args
				&key
				  interactive-function
				  report-function
				  test-function)
	       binding
	     (make-instance 'restart
			    :name name
			    :function function
			    :interactive-function interactive-function
			    :report-function report-function
			    :test-function test-function))))
    `(let ((*restart-stack*
	     (append (mapcar #'make-restart ,bindings))))
       ,@body)))

;;; The associations between restarts and conditions are represented
;;; as a list of CONS cells, each one representing a single such
;;; association.  The CAR of the CONS cell representing an association
;;; is the restart, and the CDR of the CONS cell is the condition.
;;; There is no way to disassociate a restart and a condition (other
;;; than having its extent expire), so the set of associations can
;;; only grow.  Therefore, a restart is associated with a condition if
;;; and only if in the value of this list in the current dynamic
;;; context contains the association at all. 
(defparameter *restart-condition-associations* '())

;;; FIXME: do a lot more syntax verification. 
(defmacro with-condition-restarts
    (condition-form restarts-form &body body)
  (let ((condition-var (gensym)))
    `(let* ((,condition-var ,condition-form)
	    (*restart-condition-associations*
	      (append (loop for restart in ,restarts-form
			    collect (cons restart ,condition-var)))
	      *restart-condition-associations*))
       ,@body)))

(defun association-exists-p (restart condition)
  (loop for (r . c) in *restart-condition-associations*
	when (and (eq restart r) (eq condition c))
	  return t))

(defun compute-restarts (&optional condition)
  (flet ((restart-valid-p (restart)
	   (if (null condition)
	       (null (find restart *restart-condition-associations*
			   :key #'car :test #'eq))
	       (association-exists-p restart condition))))
    (loop for restart in *restart-stack*
	  when (restart-valid-p restart)
	    collect restart)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition signaling

(defparameter *break-on-signals* nil)

;;; The stack of condition handlers is a Lisp list of of "handler
;;; groups".  A handler group is a list of "handler bindings".  A
;;; handler binding is a cons of a type and a handler.
(defparameter *handler-stack* '())

(defmacro handler-bind (bindings &body body)
  `(let ((*handler-stack* (cons (list ,@(loop for (type handler) in bindings
					      collect `(cons ',type ,handler)))
				*handler-stack*)))
     ,@body))

(defun debugger (condition)
  (declare (ignore condition))
  (format *query-io* "This is the debugger~%Type `?' for help~%")
  (loop do (let ((line (read-line *query-io*)))
	     (cond ((equal line "?")
		    (format *query-io* "this should be some help~%"))
		   ((equal line "q")
		    ;; FIXME: probably shouldn't return at all.
		    (return-from debugger nil))))))

(defun invoke-debugger (condition)
  (debugger condition))

;;; FIXME: set up restart?
(defun maybe-break-on-signals (condition)
  (when (typep condition *break-on-signals*)
    (invoke-debugger condition)))

;;; FIXME: do error checking much better than using etypecase.
(defun make-condition-from-datum-and-arguments (datum arguments default-type)
  (etypecase datum
    (symbol
       (apply #'make-condition datum arguments))
    ((or string function)
       (make-condition default-type
		       :format-control datum
		       :format-arguments arguments))
    (condition
       datum)))

;;; Find a handler for condition that does not decline.
;;; Return NIL if no handler handles. 
(defun find-and-invoke-handler (condition)
  (let ((handler-stack *handler-stack*))
    (loop for (frame . rest) on *handler-stack*
	  do (loop for (type . handler) in frame
		   do (when (typep condition type)
			(let ((*handler-stack* rest))
			  (funcall handler condition)))))))

(defun signal (datum &rest arguments)
  (let ((condition
	 (make-condition-from-datum-and-arguments
	  datum arguments 'simple-condition)))
    (maybe-break-on-signals condition)
    (find-and-invoke-handler condition)
    nil))

(defun error (datum &rest arguments)
  (let ((condition
	 (make-condition-from-datum-and-arguments
	  datum arguments 'simple-error)))
    (maybe-break-on-signals condition)
    (find-and-invoke-handler condition)
    (invoke-debugger condition)))

