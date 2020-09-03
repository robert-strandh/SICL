(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-support-clostrum
  :depends-on (:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "defun-support")
   (:file "shiftf-support")
   (:file "psetf-support")
   (:file "rotatef-support")
   (:file "destructuring-bind-support")))
