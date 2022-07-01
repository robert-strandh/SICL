(cl:in-package #:asdf-user)

(defsystem #:sicl-character-compile-time
  :depends-on ()
  :serial t
  :components
  ((:file "char-code-limit-defconstant")
   (:file "base-char-deftype")
   (:file "standard-char-deftype")
   (:file "extended-char-deftype")))
