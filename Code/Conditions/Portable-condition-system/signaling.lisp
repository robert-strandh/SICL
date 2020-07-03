;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Condition signaling

(defvar *break-on-signals* nil
  "Declares a condition type for which all signaling functions will call BREAK
before actually signaling the condition.")

(defvar *handler-clusters* '()
  "A list containing a list of all handler clusters, where a cluster is a list
of handlers established together.")

(defun signal (datum &rest arguments)
  "Coerces the provided arguments to a condition and signals it, calling all
condition handlers which match the type of the signaled condition."
  (let ((condition
          (coerce-to-condition datum arguments 'simple-condition 'signal)))
    (if (typep condition *break-on-signals*)
        (break "~A~%Break entered because of *BREAK-ON-SIGNALS*."
               condition))
    (loop for (cluster . remaining-clusters) on *handler-clusters*
          do (let ((*handler-clusters* remaining-clusters))
               (dolist (handler cluster)
                 (when (typep condition (car handler))
                   (funcall (cdr handler) condition)))))))

(defun warn (datum &rest arguments)
  "Coerces the provided arguments to a warning condition, establishes a
MUFFLE-WARNING restart, and signals the condition. If the condition is not
handled and the MUFFLE-WARNING restart is not invoked, the condition is reported
to the error output stream."
  (let ((condition (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning)
    (with-simple-restart (muffle-warning "Muffle the warning.")
      (signal condition)
      (format *error-output* "~&;; Warning:~%~A~%" condition))
    nil))

(defun error (datum &rest arguments)
  "Coerces the provided arguments to an error condition and signals it. If the
condition is not handled, invokes the debugger with that condition."
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-string datum &rest arguments)
  "Coerces the provided arguments to a warning condition, establishes a CONTINUE
restart, and signals the condition. If the condition is not handled, invokes the
debugger with that condition, allowing execution to continue if the CONTINUE
restart is invoked."
  (with-simple-restart
      (continue "~A" (apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)
