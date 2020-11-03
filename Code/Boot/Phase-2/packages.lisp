(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-2
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:load-source-file
                #:import-functions-from-host
                #:define-error-functions
                #:with-intercepted-function-cells)
  (:export #:boot))
