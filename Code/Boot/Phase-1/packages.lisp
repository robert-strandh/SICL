(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-0
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:host-load
                #:ast-eval
                #:compile-source-file
                #:load-source-file
                #:load-fasl
                #:copy-macro-functions)
  (:export #:boot))
