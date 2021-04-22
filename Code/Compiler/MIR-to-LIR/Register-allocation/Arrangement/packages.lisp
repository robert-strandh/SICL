(cl:in-package #:common-lisp-user)

(defpackage #:sicl-register-arrangement
  (:use #:common-lisp)
  (:export #:arrangement
           #:attribution
           #:copy-arrangement
           #:lexical-locations-in-register
           #:lexical-location-has-attributed-register-p
           #:lexical-location-has-attributed-stack-slot-p
           #:lexical-location-in-register-p
           #:unattributed-register-count
           #:attribute-stack-slot
           #:attribute-register-for-existing-lexical-location
           #:attribute-register-for-new-lexical-location
           #:unattribute-register
           #:trim-arrangement
           #:copy-register-attribution))
