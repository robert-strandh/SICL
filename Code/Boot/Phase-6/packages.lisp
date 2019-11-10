(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-6
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:import-function-from-host
                #:import-functions-from-host)
  (:export #:boot))
