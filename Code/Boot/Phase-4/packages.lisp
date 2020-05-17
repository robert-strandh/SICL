(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-4
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:load-fasl
                #:load-source
                #:import-function-from-host
                #:import-functions-from-host)
  (:export #:boot))
