(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:environment
           #:name
           #:client
           #:boot
           #:repl
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5 #:ecs
           #:*e0* #:*e1* #:*e2* #:*e3* #:*e4* #:*e5* #:*ecs*
           #:import-functions-from-host
           #:define-error-functions
           #:load-source-file
           #:load-asdf-system
           #:ast-eval
           #:copy-macro-functions
           #:define-environment-functions
           #:create-accessor-defgenerics
           #:create-mop-classes
           #:with-intercepted-function-cells
           #:overridden-function-cells
           #:header
           #:original-function
           #:bt))
