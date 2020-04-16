(cl:in-package #:cleavir-cst-to-ast)

(defun function-info (environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (result (cleavir-env:function-info environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'cleavir-env:no-function-info
				  :name function-name
                                  :origin (cst:source function-name-cst))
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
    (if (and (consp ftype)
             (eq (car ftype) 'cl:function)
             (consp (cdr ftype))
             (cleavir-code-utilities:proper-list-p (cadr ftype)))
        (destructuring-bind (lambda-list &optional (values '*)) (cdr ftype)
          (declare (ignore values))
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
                         ;; Syntax check: Should be (keyword type)
                         (if (and (consp item)
                                  (consp (cdr item))
                                  (null (cddr item)))
                             (push (copy-list item) keys)
                             (give-up))))))
                finally
                   (return (values t (nreverse required) (nreverse optional)
                                   restp rest keysp (nreverse keys) aok-p))))
        (give-up))))

;;; This function takes a type specifier and returns a (FUNCTION ...) type specifier.
(defun normalize-ftype (ftype)
  (if (consp ftype)
      (case (car ftype)
        ((function)
         ;; This may be from a user declaration, so don't accept it out of hand
         (let ((len (cleavir-code-utilities:proper-list-length ftype)))
           (if (and len (<= len 1 3))
               (destructuring-bind (&optional (lambda-list '*) (ret '*))
                   ftype
                 `(function ,lambda-list ,ret))
               '(function * *))))
        ((and)
         (if (cleavir-code-utilities:proper-list-p ftype)
             ;; We just collect the first valid and helpful one. TODO: Actually merge?
             (loop for sub in (rest ftype)
                   for normalized = (normalize-ftype sub)
                   unless (equal normalized '(function * *))
                     return normalized
                   finally (return '(function * *)))))
        (otherwise '(function * *)))
      '(function * *)))

(defun function-type (info)
  (normalize-ftype (cleavir-env:type info)))
