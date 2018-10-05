(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot
  (:use #:common-lisp)
  (:export #:boot
           #:e0 #:e1 #:e2 #:e3 #:e4
           #:load-file
           #:import-function-from-host
           #:import-functions-from-host
           #:import-package-from-host))
