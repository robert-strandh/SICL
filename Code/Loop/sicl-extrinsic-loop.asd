(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-loop
  :depends-on (:sicl-loop-support)
  :serial t
  :components
  ((:file "shadow-export")
   (:file "loop-defmacro")))
