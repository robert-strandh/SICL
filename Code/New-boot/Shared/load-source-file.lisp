(cl:in-package #:sicl-new-boot)

(defun eval-cst (client cst environment)
  (let* ((ast (cst-to-ast client cst environment))
         (top-level-function (cbae:compile-ast client ast environment)))
    (funcall top-level-function)))

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
