(defpackage #:sicl-condition-signaling
    (:use #:common-lisp)
  (:shadow #:error
	   #:cerror
	   #:signal
	   #:handler-bind
	   #:handler-case
	   #:ignore-errors
	   #:invoke-debugger
	   #:*debugger-hook*
	   #:*break-on-signals*))

(in-package #:sicl-condition-signaling)

(defparameter *break-on-signals* nil)

;;; The stack of condition handlers is a Lisp list of
;;; of "handler binds".  A handler bind is a list of 
;;; "handler bindings".  A handler binding is a cons
;;; of a type and a handler. 
(defparameter *handler-stack* '())

;;; Find a handler for a particular condition.
;;; If a handler is found, return two values:
;;; the handler, and the bind stack that just
;;; excludes the handler bind in which the 
;;; appropriate handler was found.  If a handler 
;;; is not found, return NIL.
(defun find-handler (condition handler-stack)
  (loop for remaining-stack on handler-stack
	do (loop for binding in (car remaining-stack)
		 when (typep condition (car binding))
		   do (return-from find-handler
			(values (cdr binding) (cdr remaining-stack))))))

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
    (loop do (multiple-value-bind (handler remaining-handler-stack)
		 (find-handler condition handler-stack)
	       (when (null handler)
		 (return nil))
	       ;; We have a handler.  
	       ;; Call it, excluding the current
	       ;; handler bind.
	       (let ((*handler-stack* remaining-handler-stack))
		 (funcall handler condition))
	       ;; Come here if handler declined, i.e. returned.
	       ;; Continue the search.
	       (setf handler-stack remaining-handler-stack)))))

(defun signal (datum &rest arguments)
  (let ((condition
	 (make-condition-from-datum-and-arguments datum arguments 'simple-condition)))
    (maybe-break-on-signals condition)
    (find-and-invoke-handler condition)
    nil))

(defun error (datum &rest arguments)
  (let ((condition
	 (make-condition-from-datum-and-arguments datum arguments 'simple-condition)))
    (maybe-break-on-signals condition)
    (find-and-invoke-handler condition)
    (invoke-debugger condition)))


