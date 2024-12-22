(cl:in-package #:asdf-user)

(defsystem "sicl-data-and-control-flow-setf-expanders"
  :serial t
  :components
  ((:file "packages")
   (:file "get-setf-expansion-defun")
   (:file "values-define-setf-expander")))
