(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-internationalization
  (:nicknames #:cleavir-i18n)
  (:shadow #:condition)
  (:use #:common-lisp)
  (:export
   #:condition
   #:locale
   #:*locale*
   ))
