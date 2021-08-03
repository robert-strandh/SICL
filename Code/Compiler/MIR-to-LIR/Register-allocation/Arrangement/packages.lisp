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
           #:arrangements-compatible-p
           #:unattributed-register-count
           #:first-stack-slot-past-arrangement
           #:first-free-register
           #:attribute-stack-slot
           #:attribute-register-for-existing-lexical-location
           #:attribute-register-for-new-lexical-location
           #:reattribute-register
           #:unattribute-register
           #:find-attribution
           #:map-attributions
           #:trim-arrangement
           #:copy-register-attribution
           #:frozen-p))
