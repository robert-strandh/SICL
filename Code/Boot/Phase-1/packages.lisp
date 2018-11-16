(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-1
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:load-file
                #:import-function-from-host
                #:import-functions-from-host
                #:import-package-from-host)
  (:export #:boot-phase-1))
