(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-sequence-functions
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:load-source-file
                #:import-functions-from-host
                #:define-error-functions)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:boot))
