(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow
   #:package
   #:packagep
   #:*package*
   #:package-name
   #:package-nicknames
   #:package-shadowing-symbols
   #:package-use-list
   #:package-used-by-list
   #:package-error
   #:package-error-package
   #:rename-package
   #:make-package
   #:import
   #:intern
   #:unintern
   #:find-symbol
   #:export
   #:unexport
   #:shadow
   #:shadowing-import
   #:unuse-package
   #:use-package
   #:do-symbols
   #:do-external-symbols)
  (:export #:package
           #:*package*
           #:export
           #:find-symbol
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
           #:unintern
           #:package-name
           #:package-nicknames
           #:package-shadowing-symbols
           #:package-use-list
           #:package-used-by-list
           #:packagep
           #:package-error
           #:package-error-package))
