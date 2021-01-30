(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-1
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:host-load
                #:compile-source-file
                #:load-source-file
                #:load-fasl
                #:copy-macro-functions)
  (:export #:boot))
