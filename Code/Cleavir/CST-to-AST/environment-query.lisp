(cl:in-package #:cleavir-cst-to-ast)

(defun describe-function (client environment function-name)
  (let ((result (trucler:describe-function client environment function-name)))
    (loop while (null result)
          do (restart-case (error 'trucler:no-function-description
                                  :name function-name)
               (consider-global ()
                 :report (lambda (stream)
                           (format stream
                                   "Treat it as the name of a global function."))
                 (return-from describe-function
                   (make-instance 'trucler:global-function-description
                     :name function-name)))
               (substitute (new-function-name)
                 :report (lambda (stream)
                           (format stream "Substitute a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq result
                       (trucler:describe-function
                        client environment new-function-name)))))
    result))

(defun describe-tag (client environment tag-name)
  (let ((result (trucler:describe-tag client environment tag-name)))
    (loop while (null result)
          do (restart-case (error 'trucler:no-tag-description
                                  :name tag-name)
               (substitute (new-tag-name)
                 :report (lambda (stream)
                           (format stream "Substitute a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq result
                       (trucler:describe-tag
                        client environment new-tag-name)))))
    result))

(defmethod declaration-proclamations (client environment)
  '())
