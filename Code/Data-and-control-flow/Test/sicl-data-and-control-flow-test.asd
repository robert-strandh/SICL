(cl:in-package #:asdf-user)

(push :sicl-data-and-control-flow-extra
      (asdf/component:component-sideway-dependencies
       (asdf:find-system '#:sicl-data-and-control-flow)))

(defsystem :sicl-data-and-control-flow-test
  :depends-on (:sicl-data-and-control-flow)
  :serial t
  :components
  ((:file "test")))
