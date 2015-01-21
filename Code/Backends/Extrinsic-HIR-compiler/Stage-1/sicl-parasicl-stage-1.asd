(cl:in-package #:asdf-user)

(defsystem :sicl-parasicl-stage-1
  :depends-on (:sicl-extrinsic-hir-compiler)
  :serial t
  :components
  ((:file "create-stage-1")
   (:file "fill-global-environment")
   (:file "import-from-stage-0")))
