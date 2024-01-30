(cl:in-package #:sicl-new-boot-phase-2)

;;; The arguments received by ENSURE-GENERIC-FUNCTION are not quite as
;;; documented, because we can make certain shortcuts here, given that
;;; this function is going to be called only from the expansion of
;;; DEFGENERIC, and we have full control over that expansion.
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
          (assert (null (clo:fboundp client e2 name)))
          (setf (clo:fdefinition client e2 name)
                (make-instance
                    (clo:find-class client e1 generic-function-class t)
                  :argument-precedence-order argument-precedence-order
                  :lambda-list lambda-list
                  :method-class (clo:find-class client e1 method-class t)
                  :method-combination method-combination)))))

;;; The arguments received by ENSURE-CLASS are not quite as
;;; documented, because we can make certain shortcuts here, given that
;;; this function is going to be called only from the expansion of
;;; DEFCLASS, and we have full control over that expansion.
(defun define-ensure-class (client e1 e2)
  (setf (clo:fdefinition client e2 'ensure-class)
        (lambda (name
                 &key
                   direct-default-initargs
                   direct-slots
                   direct-superclasses
                   metaclass)
          (let ((superclasses
                  (loop for class-name in direct-superclasses
                        collect (clo:find-class client e2 class-name t))))
            (setf (clo:find-class client e2 name)
                  (make-instance
                      (clo:find-class client e1 metaclass t)
                    :direct-default-initargs direct-default-initargs
                    :direct-slots direct-slots
                    :direct-superclasse superclasses
                    :name name))))))

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
     client environment "sicl-clos-package")
    (define-ensure-generic-function client (sb:e1 boot) environment))
  boot)
