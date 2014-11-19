(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-internationalization
  (:nicknames #:cleavir-i18n)
  (:shadow #:condition)
  (:use #:common-lisp)
  (:export
   #:condition
   #:report-condition
   #:locale
   #:*locale*
   #:language
   #:english
   #:francais
   #:svenska
   #:deutsch
   #:tieng-viet
   ))
