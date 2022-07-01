(cl:in-package #:asdf-user)

(defsystem #:sicl-character-load-time
  :depends-on ()
  :serial t
  :components
  ((:file "character-defclass")))
