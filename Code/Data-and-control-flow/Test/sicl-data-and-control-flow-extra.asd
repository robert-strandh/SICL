(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-extra
  :depends-on (:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "shadow")))
