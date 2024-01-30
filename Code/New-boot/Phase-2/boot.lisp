(cl:in-package #:sicl-new-boot-phase-2)

(defun define-ensure-generic-function (client e1 e2)
  (setf (clo:fdefinition client e2 'ensure-generic-function)
        (lambda (name
                 &key
                   argument-precedence-order
                   declare
                   documentation
                   environment
                   generic-function-class
                   lambda-list
                   method-class
                   method-combination)
          (declare (ignore declare documentation environment))
          (check-type generic-function-class symbol)
          (check-type method-class symbol)
          (assert (null (clo:fboundp client e2 name)))
          (setf (clo:fdefinition client e2 name)
                (make-instance
                    (clo:find-class client e1 generic-function-class)
                  :argument-precedence-order argument-precedence-order
                  :lambda-list lambda-list
                  :method-class (clo:find-class client e1 method-class)
                  :method-combination method-combination)))))

(defun boot (boot)
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment)))
    (sb:define-package-functions client global-environment)
    (setf (sb:e2 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))    (sb:ensure-asdf-system
     client environment "sicl-environment-package")
    (let ((environment-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*ENVIRONMENT*"))
          (client-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*CLIENT*")))
      (clo:make-variable
       client global-environment environment-symbol global-environment)
      (clo:make-variable
       client global-environment client-symbol client))
    (sb:ensure-asdf-system
     client environment "clostrophilia-package")
    (sb:ensure-asdf-system
     client environment "sicl-clos-package"))
  boot)
