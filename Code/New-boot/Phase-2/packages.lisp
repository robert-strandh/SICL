(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-phase-2
  (:use #:common-lisp)
  (:local-nicknames
   (#:env #:sicl-environment)
   (#:sb #:sicl-new-boot)
   (#:cb #:common-boot)
   (#:clo #:clostrum)
   (#:cmd #:common-macro-definitions)
   (#:abp #:architecture.builder-protocol))
  (:export
   #:boot))
