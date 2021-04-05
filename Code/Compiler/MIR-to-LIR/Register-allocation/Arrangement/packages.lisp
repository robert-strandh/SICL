(cl:in-package #:common-lisp-user)

(defpackage #:sicl-register-arrangement
  (:use #:common-lisp)
  (:export #:copy-arrangement
           #:lexical-locations-in-register
           #:lexical-location-has-attributed-register-p
           #:unattributed-register-exists-p
           #:ensure-stack-slot
           #:delete-attribution))

           
