(cl:in-package #:asdf-user)

(defsystem #:cleavir-cst-to-ast-test
  :depends-on (#:cleavir-cst-to-ast
               #:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "ast-from-string")
   (:file "test" :around-compile
          (lambda (thunk)
            (let ((*readtable* cleavir-io:*io-readtable*)
                  (cleavir-ast:*policy* nil))
              (funcall thunk))))))
