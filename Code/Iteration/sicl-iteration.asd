(cl:in-package #:asdf-user)

(defsystem :sicl-iteration
  :depends-on (:cleavir-code-utilities
               :acclimation
               :sicl-iteration-support)
  :serial t
  :components
  ((:file "dotimes-defmacro")
   (:file "dolist-defmacro")
   (:file "do-dostar-defmacro")))
