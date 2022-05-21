(cl:in-package #:common-lisp-user)

(defpackage #:sicl-symbol
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
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
