(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-4
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:import-functions-from-host
                #:define-error-functions
                #:load-source-file
                #:with-intercepted-function-cells
                #:make-client)
  (:export #:boot))
