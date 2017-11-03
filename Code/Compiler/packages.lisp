(defpackage #:sicl-program
  (:use #:common-lisp)
  (:export
   #:program
   #:backend
   #:registers
   #:*program*
   #:touch
   #:instruction-graph
   #:no-error-successors
   #:simplified-instructions
   #:remove-nop-instructions
   #:unique-constants
   #:basic-blocks
   #:dominance
   #:no-constant-inputs
   #:initial-transformations
   #:convert-constant
   #:convert-to-lir
   #:required-register #:preferred-register
   #:spill-cost
   #:collect-dynamic-locations
   ))

(defpackage #:sicl-type-proclamations
  (:use #:common-lisp)
  (:shadowing-import-from #:sicl-global-environment
			  #:proclaim))

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:shadow #:compile-file)
  (:export
   #:*backend*
   #:compile-file
   #:type-inference
   #:trim-instruction-graph
   ))
