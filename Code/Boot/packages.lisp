(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot
  (:use #:common-lisp)
  (:export #:environment
           #:client
           #:boot
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5 #:e6 #:e7
           #:*e0* #:*e1* #:*e2* #:*e3* #:*e4* #:*e5* #:*e6* #:*e7*
           #:import-function-from-host
           #:import-functions-from-host
           #:import-package-from-host
           #:import-class-from-host
           #:tie-code-object
           #:load-fasl
           #:with-straddled-function-definitions
           #:enable-defgeneric
           #:enable-method-combinations
           #:create-mop-classes
           #:define-accessor-generic-functions))
