(cl:in-package #:common-lisp-user)

(defpackage #:sicl-call-site-manager
  (:use #:common-lisp)
  (:export
   #:add-call-site
   #:create-trampoline-snippet))
