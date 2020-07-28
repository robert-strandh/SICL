(cl:in-package #:asdf-user)

(defsystem #:target
  :depends-on (#:cleavir-code-utilities)
  :serial t
  :components
  ((:file "target-common-lisp-package")
   (:file "target-cons-package")
   (:file "target-evaluation-and-compilation-package")
   (:file "target-data-and-control-flow-package")
   (:file "load-target-macros")))
