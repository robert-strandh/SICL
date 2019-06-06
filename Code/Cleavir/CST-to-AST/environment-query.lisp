(cl:in-package #:cleavir-cst-to-ast)

(defun function-info (environment function-name)
  (let ((result (cleavir-env:function-info environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'cleavir-env:no-function-info
				  :name function-name)
	       (consider-global ()
		 :report (lambda (stream)
			   (format stream
				   "Treat it as the name of a global function."))
		 (return-from function-info
		   (make-instance 'cleavir-env:global-function-info
		     :name function-name)))
	       (substitute (new-function-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (cleavir-env:function-info environment new-function-name)))))
    result))

(defun tag-info (environment tag-name)
  (let ((result (cleavir-env:tag-info environment tag-name)))
    (loop while (null result)
	  do (restart-case (error 'cleavir-env:no-tag-info
				  :name tag-name)
	       (substitute (new-tag-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (cleavir-env:tag-info environment new-tag-name)))))
    result))

;;; FIXME: Needs cleanup and to be moved.
;;; This function takes a (FUNCTION ...) type specifier and returns
;;; validp, required, optional, restp, rest, keysp, keys, allow-other-keys-p as values.
;;; We could signal warnings/errors on malformed function types, but we're getting these
;;; from the client, which might want to do its own validation, ahead of time.
(defun parse-function-type (ftype)
  (flet ((give-up ()
           (return-from parse-function-type nil)))
    (handler-case
        (destructuring-bind (fn &optional (lambda-list '*) (values '*))
            ftype
          (declare (ignore fn values))
          (loop with state = :required
                with required with optional with restp
                with rest with keysp with keys with aok-p
                for item in lambda-list
                do (case item
                     ((&optional)
                      (if (eq state :required)
                          (setf state '&optional)
                          (give-up)))
                     ((&rest)
                      (if (member state '(:required &optional))
                          (setf state '&rest)
                          (give-up)))
                     ((&key)
                      (if (member state '(:required &optional &rest))
                          (setf state '&key keysp t)
                          (give-up)))
                     ((&allow-other-keys)
                      (if (eq state '&key)
                          (setf state '&allow-other-keys aok-p t)
                          (give-up)))
                     (t ; actual type thing
                      (ecase state
                        ((:required) (push item required))
                        ((&optional) (push item optional))
                        ((&rest) (cond (restp (give-up))
                                       (t (setf restp t) (setf rest item))))
                        ((&key)
                         ;; We want to signal an error/give up if the syntax is bad.
                         (destructuring-bind (keyword type) item
                           (push (list keyword type) keys))))))
                finally
                   (return (values t (nreverse required) (nreverse optional)
                                   restp rest keysp (nreverse keys) aok-p))))
      (error () (give-up)))))

;;; This function takes a type specifier and returns a (FUNCTION ...) type specifier.
(defun normalize-ftype (ftype)
  (if (consp ftype)
      (handler-case
          (case (car ftype)
            ((function) ftype)
            ((and)
             ;; We just collect the first valid and helpful one. TODO: Actually merge?
             (loop for sub in (rest ftype)
                   for normalized = (normalize-ftype sub)
                   unless (equal normalized '(function * *))
                     return normalized
                   finally (return '(function * *))))
            (t '(function * *)))
        (error () '(function * *)))
      '(function * *)))

(defun function-type (info)
  (normalize-ftype (cleavir-env:type info)))
