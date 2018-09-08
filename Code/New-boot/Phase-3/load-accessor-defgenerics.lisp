(cl:in-package #:sicl-new-boot-phase-3)

(defun ensure-generic-function-phase-3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e4 sicl-new-boot:e4)) boot
    (let* ((gf-class-name 'standard-generic-function)
           (make-instance (sicl-genv:fdefinition 'make-instance e3)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function e4)
            (lambda (function-name &rest arguments
                     &key environment
                     &allow-other-keys)
              (let ((args (copy-list arguments)))
                (loop while (remf args :environment))
                (if (sicl-genv:fboundp function-name environment)
                    (sicl-genv:fdefinition function-name environment)
                    (setf (sicl-genv:fdefinition function-name environment)
                          (apply make-instance
                                 gf-class-name
                                 :name function-name
                                 args)))))))))

(defun activate-generic-function-initialization (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (import-functions-from-host
     '(set-difference stringp
       cleavir-code-utilities:parse-generic-function-lambda-list
       cleavir-code-utilities:required
       make-list)
     e3)
    ;; We may regret having defined FIND-CLASS this way in E3.
    (setf (sicl-genv:fdefinition 'find-class e3)
          (lambda (class-name)
            (sicl-genv:find-class class-name e2)))
    (load-file "CLOS/generic-function-initialization-support.lisp" e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::invalidate-discriminating-function e3)
          #'identity)
    (load-file "CLOS/generic-function-initialization-defmethods.lisp" e3)))

(defun load-accessor-defgenerics (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (ensure-generic-function-phase-3 boot)
    (activate-generic-function-initialization boot)
    (sicl-minimal-extrinsic-environment:import-package-from-host
     'sicl-clos e4)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defgeneric-expander e4)))
