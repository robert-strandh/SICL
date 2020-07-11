;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Condition signaling

(defvar *break-on-signals* nil)

(defvar *handler-clusters* '())

(defun signal (datum &rest arguments)
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
  (let ((condition (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning)
    (with-simple-restart (muffle-warning "Muffle the warning.")
      (signal condition)
      (format *error-output* "~&;; Warning:~%~A~%" condition))
    nil))

(defun error (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart
      (continue "~A" (apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)
