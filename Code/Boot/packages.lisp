(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:environment
           #:name
           #:client
           #:boot
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5 #:e6 #:e7
           #:*e0* #:*e1* #:*e2* #:*e3* #:*e4* #:*e5* #:*e6* #:*e7*
           #:import-functions-from-host
           #:define-error-functions
           #:compile-source-file
           #:load-source-file
           #:ast-eval
           #:load-fasl
           #:copy-macro-functions
           #:define-environment-functions
           #:create-accessor-defgenerics
           #:create-mop-classes
           #:with-intercepted-function-cells
           #:overridden-function-cells
           #:header))
