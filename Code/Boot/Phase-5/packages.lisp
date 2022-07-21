(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-5
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:load-source-file
                #:import-functions-from-host
                #:ensure-asdf-system
                #:with-intercepted-function-cells)
  (:export #:boot #:environment))
