(in-package #:sicl-conditions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition signaling

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

