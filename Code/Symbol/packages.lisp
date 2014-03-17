(cl:in-package #:common-lisp-user)

(defpackage #:sicl-symbol
  (:use #:common-lisp)
  (:shadow #:package)
  (:export
   #:symbol
   #:symbolp
   #:keyword
   #:keywordp
   #:symbol-name
   #:package
   #:symbol-package
   #:make-symbol
   #:copy-symbol
   #:gensym
   #:*gensym-counter*
   #:gentemp
   ))
