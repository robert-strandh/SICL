(cl:in-package #:common-lisp-user)

(defpackage #:sicl-symbol
  (:use #:common-lisp)
  (:export #:symbol
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
           #:variable-cell))
