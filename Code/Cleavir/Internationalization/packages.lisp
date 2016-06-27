(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-internationalization
  (:shadow #:condition)
  (:use #:common-lisp)
  (:export
   #:condition
   #:report-condition
   #:locale
   #:*locale*
   #:long-day-name
   #:short-day-name
   #:language
   #:english
   #:francais
   #:svenska
   #:deutsch
   #:tieng-viet
   ))
