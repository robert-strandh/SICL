(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-phase-0
  (:use #:common-lisp)
  (:import-from #:sicl-new-boot
                #:load-file
                #:import-function-from-host
                #:import-functions-from-host
                #:import-package-from-host)
  (:export #:boot-phase-0))
