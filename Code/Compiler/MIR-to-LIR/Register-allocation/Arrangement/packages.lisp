(cl:in-package #:common-lisp-user)

(defpackage #:sicl-register-arrangement
  (:use #:common-lisp)
  (:export #:copy-arrangement
           #:lexical-locations-in-register
           #:ensure-stack-slot
           #:delete-attribution))

           
