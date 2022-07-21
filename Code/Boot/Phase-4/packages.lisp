(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-4
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:import-from #:sicl-boot
                #:import-functions-from-host
                #:load-source-file
                #:ensure-asdf-system
                #:with-intercepted-function-cells)
  (:export #:boot))
