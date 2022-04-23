(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-sequence-functions
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:import-functions-from-host
                #:define-error-functions)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:boot))
