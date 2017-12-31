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
           #:find-symbol)))
