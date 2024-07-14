(cl:in-package #:sicl-new-boot)

(defvar *use-maclina-p* t)

(defun maclina-eval-cst (client cst environment)
  (let* ((global-environment (trucler:global-environment client environment))
         (compilation-environment
           (make-instance 'clostrum-basic:compilation-environment
             :parent global-environment))
         (cmd:*client* client)
         (maclina.machine:*client* client)
         (form (cst:raw cst)))
    (maclina.compile:eval form compilation-environment)))

(defun maclina-eval (form global-environment)
  (let* ((compilation-environment
           (make-instance 'clostrum-basic:compilation-environment
             :parent global-environment))
         (client (make-instance 'client))
         (cmd:*client* client)
         (maclina.machine:*client* client))
    (maclina.compile:eval form compilation-environment)))

(defun ast-evaluator-eval-cst (client cst environment)
  (let* ((ast (cst-to-ast client cst environment))
         (top-level-function (cbae:compile-ast client ast environment)))
    (funcall top-level-function)))

(defun eval-cst (client cst environment)
  (if *use-maclina-p*
      (maclina-eval-cst client cst environment)
      (ast-evaluator-eval-cst client cst environment)))

(defun load-stream (client stream environment)
  (let ((eclector.base:*client* client)
        (abp:*builder* (cb::make-builder client environment))
        (undefined-functions '()))
    (handler-bind
        ((common-boot:no-function-description
           (lambda (condition)
             (let ((restart (find-restart 'muffle-warning condition)))
               (unless (null restart)
                 (pushnew condition undefined-functions
                          :key #'common-boot:name
                          :test (lambda (condition-name name)
                                  (equal condition-name name)))
                 (invoke-restart restart))))))
      (loop with eof = (list nil)
            for cst = (eclector.concrete-syntax-tree:read stream nil eof)
            until (eq cst eof)
            do (eval-cst client cst environment)))
    (loop with global-environment
            = (trucler:global-environment client environment)
          for condition in undefined-functions
          do (unless (clo:fboundp
                      client global-environment (common-boot:name condition))
               (warn condition)))))

(defun load-source-file (client filename environment)
  (format *trace-output* "Loading ~s into ~s~%"
          filename (name (trucler:global-environment client environment)))
  (finish-output *trace-output*)
  (sicl-source-tracking:with-source-tracking-stream-from-file (stream filename)
    (load-stream client stream environment)))
