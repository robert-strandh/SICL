(cl:in-package #:asdf-user)

(defsystem :sicl-loop
  :depends-on (:sicl-loop-support)
  :serial t
  :components
  ((:file "loop-defmacro")))
