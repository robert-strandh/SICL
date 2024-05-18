(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot
  (:use #:common-lisp)
  (:shadow #:class)
  (:local-nicknames
   (#:env #:sicl-environment)
   (#:abp #:architecture.builder-protocol)
   (#:cmd #:common-macro-definitions)
   (#:clo #:clostrum)
   (#:cb #:common-boot)
   (#:cbae #:common-boot-ast-evaluator))
  (:shadow #:symbol-package)
  (:export
   #:boot #:e1 #:e2 #:e3 #:packages #:symbol-package
   #:client #:environment
   #:name
   #:define-backquote-macros
   #:ensure-asdf-system
   #:import-host-functions
   #:define-setf-functions
   #:intern-parcl-symbol
   #:define-package-functions
   #:define-environment-functions
   #:import-khazern
   #:enable-parcl-symbols
   #:read-symbol-components
   #:allocate-general-instance
   #:standard-instance-access
   #:header #:class #:rack
   #:eval-cst
   #:add-package-local-nickname))
