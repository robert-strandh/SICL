(cl:in-package :common-lisp-user)

(asdf:defsystem :sicl-clos
  :depends-on (:sicl-additional-types
	       :sicl-additional-conditions
	       :cleavir-code-utilities)
  :components
  ((:file "packages"
    :depends-on ())))
