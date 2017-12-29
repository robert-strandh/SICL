(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:export
   #:package
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
