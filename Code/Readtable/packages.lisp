(cl:in-package #:common-lisp-user)

(defpackage #:sicl-readtable
  (:use #:common-lisp)
  (:shadow #:copy-readtable
           #:make-dispatch-macro-character
           #:readtable-case
           #:readtablep
           #:get-macro-character
           #:set-macro-character
           #:get-dispatch-macro-character
           #:set-dispatch-macro-character
           #:set-syntax-from-char)
  (:export #:copy-readtable
           #:copy-readtable-into
           #:make-dispatch-macro-character
           #:readtable-case
           #:readtablep
           #:get-macro-character
           #:set-macro-character
           #:get-dispatch-macro-character
           #:set-dispatch-macro-character
           #:set-syntax-from-char))
