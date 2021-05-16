(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:environment
           #:name
           #:client
           #:boot
           #:repl
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5
           #:*e0* #:*e1* #:*e2* #:*e3* #:*e4* #:*e5*
           #:import-functions-from-host
           #:define-error-functions
           #:compile-source-file
           #:load-source-file
           #:load-asdf-system
           #:new-load-asdf-system
           #:ast-eval
           #:load-fasl
           #:copy-macro-functions
           #:define-environment-functions
           #:create-accessor-defgenerics
           #:create-mop-classes
           #:with-intercepted-function-cells
           #:overridden-function-cells
           #:header))
