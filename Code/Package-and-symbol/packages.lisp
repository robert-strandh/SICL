(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow #:find-package)
  (:export . #.asdf-user:*sicl-package-string-designators*))

(defpackage #:sicl-symbol
  (:use #:common-lisp)
  (:shadow
   #:package
   .
   #+sicl-host-test (#:symbol)
   #-sicl-host-test ())
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
