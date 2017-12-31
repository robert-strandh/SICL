(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow
   .
   #-sicl (#:package
           #:*package*
           #:package-name
           #:package-shadowing-symbols
           #:package-use-list
           #:package-used-by-list
           #:package-error
           #:make-package
           #:intern
           #:find-symbol))
  (:export #:package
           #:export
           #:find-symbol
           #:import
           #:rename-package
           #:shadow
           #:shadowing-import
           #:make-package
           #:with-package-iterator
           #:unexport
           #:unintern
           #:unuse-package
           #:use-package
           #:defpackage
           #:do-symbols
           #:do-external-symbols
           #:intern
           #:package-name
           #:package-nicknames
           #:package-shadowing-symbols
           #:package-use-list
           #:package-used-by-list
           #:packagep))

(defpackage #:sicl-symbol
  (:use #:common-lisp)
  (:shadow
   .
   #-sicl (#:symbol))
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
           #:gentemp))
