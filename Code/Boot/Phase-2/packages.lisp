(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-2
  (:use #:common-lisp)
  (:import-from #:sicl-boot
                #:load-file
                #:load-file-protected
                #:import-function-from-host
                #:import-functions-from-host
                #:import-package-from-host
                #:import-class-from-host
                #:define-error-function)
  (:export #:boot-phase-2))
