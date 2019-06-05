(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-3
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:load-fasl
                #:import-function-from-host)
  (:export #:boot))
