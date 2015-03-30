(cl:in-package #:asdf-user)

(defsystem :sicl-stream
  :depends-on (:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")))
