(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:environment
           #:name
           #:client
           #:boot
           #:repl
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5 #:ecs #:e*
           #:*e0* #:*e1* #:*e2* #:*e3* #:*e4* #:*e5* #:*ecs*
           #:import-functions-from-host
           #:load-source-file
           #:ast-eval
           #:copy-macro-functions
           #:define-environment-functions
           #:create-accessor-defgenerics
           #:create-classes
           #:with-intercepted-function-cells
           #:overridden-function-cells
           #:header
           #:function-header
           #:simple-function-header
           #:generic-function-header
           #:method-header
           #:class-header
           #:array-header
           #:vector-header
           #:string-header
           #:hash-table-header
           #:run-time-environment-header
           #:original-function
           #:with-temporary-function-imports
           #:pointer
           #:bt))
