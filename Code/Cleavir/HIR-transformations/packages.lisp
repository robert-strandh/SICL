(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:eliminate-catches
   #:load-time-value-is-constant-p
   #:load-time-value-constant
   #:convert-constant-to-immediate
   #:convert-constants-to-immediate
   #:eliminate-load-time-value-inputs
   #:eliminate-typeq
   #:type-inference
   #:process-captured-variables
   #:segregate-only
   #:read-only-location-p
   #:simplify-boxes
   #:compute-instruction-owners
   #:compute-location-owners
   #:segregate-lexicals
   #:hir-transformations
   #:introduce-intermediate
   #:eliminate-superfluous-temporaries
   #:maybe-eliminate
   #:replace-inputs
   #:replace-outputs)
  (:export
   #:compute-destinies
   #:find-enclose-destinies
   #:destiny-find-encloses
   #:discern-trappers
   #:discern-sharing)
  (:export
   #:build-function-dag
   #:enter-instruction
   #:enclose-instruction
   #:dag-nodes
   #:parents
   #:children
   #:remove-enclose-from-function-dag
   #:add-enclose-to-parents)
  (:export
   maybe-convert-values-location)
  (:export
   copy-propagate-1
   copy-propagate-instruction)
  (:export
   eliminate-redundant-typeqs))
