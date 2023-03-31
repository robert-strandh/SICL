(cl:in-package #:sicl-future-cst-to-ast)

(defun describe-function (client environment cooked-function-name)
  (let* ((raw-function-name (c:raw cooked-function-name))
         (result (trucler:describe-function
                  client environment raw-function-name)))
    (loop while (null result)
          do (restart-case (error "No function description for ~s"
                                  raw-function-name)
               (consider-global ()
                 :report (lambda (stream)
                           (format stream
                                   "Treat it as the name of a global function."))
                 (return-from describe-function
                   (make-instance 'trucler:global-function-description
                     :origin (c:origin cooked-function-name)
                     :name raw-function-name)))
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
          do (restart-case (error "No tag name ~s" tag-name)
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
