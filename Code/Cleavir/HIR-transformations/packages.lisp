(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:eliminate-catches
   #:segregate-only
   #:segregate-lexicals
   #:replace-inputs
   #:replace-outputs
   #:compute-instruction-owners
   #:compute-location-owners
   #:build-function-dag
   #:dag-nodes
   #:remove-enclose-from-function-dag
   #:add-enclose-to-parents
   #:enter-instruction
   #:enclose-instruction
   #:parents
   #:children
   #:find-enclose-destinies
   #:destiny-find-encloses
   #:compute-destinies
   #:replace-aliases))
