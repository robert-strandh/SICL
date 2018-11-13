(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot
  (:use #:common-lisp)
  (:export #:boot
           #:environment
           #:e0 #:e1 #:e2 #:e3 #:e4 #:e5
           #:load-file
           #:load-file-protected
           #:import-function-from-host
           #:import-functions-from-host
           #:import-package-from-host
           #:import-class-from-host
           #:define-error-function))
