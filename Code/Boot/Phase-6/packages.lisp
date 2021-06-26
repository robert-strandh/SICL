(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-6
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:define-error-functions
                #:load-source-file
                #:load-asdf-system
                #:with-intercepted-function-cells)
  (:export #:boot))
