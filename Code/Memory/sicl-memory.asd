(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-memory
  :components
  ((:file "packages")
   (:file "image" :depends-on ("packages"))
   (:file "heap" :depends-on ("packages" "image"))))
