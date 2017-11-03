(defpackage #:sicl-word
  (:use #:common-lisp)
  (:export
   #:word
   #:memalloc #:memref #:memset
   #:u+ #:u- #:s+ #:s- #:neg
   #:u* #:s*
   #:lshift #:ashift
   #:& #:ior #:xor #:~
   #:== #:s< #:s<= #:u< #:u<=
   #:<< #:>>
   #:halt
   ))

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

(defpackage #:sicl-compiler-types
  (:use #:common-lisp)
  (:export
   #:type-descriptor-from-type
   #:rational-descriptor
   #:short-float-descriptor
   #:single-float-descriptor
   #:double-float-descriptor
   #:long-float-descriptor
   #:complex-descriptor
   #:array-t-descriptor
   #:array-single-float-descriptor
   #:array-double-float-descriptor
   #:array-character-descriptor
   #:array-bit-descriptor
   #:array-unsigned-byte-8-descriptor
   #:array-unsigned-byte-32-descriptor
   #:array-signed-byte-32-descriptor
   #:array-unsigned-byte-64-descriptor
   #:array-signed-byte-64-descriptor
   #:null-descriptor
   #:others-descriptor
   #:type-descriptor-and
   #:type-descriptor-or
   #:type-descriptor-diff
   #:type-descriptor
   #:make-t-type-descriptor
   #:make-t-type-map
   #:type-map-or
   #:type-map-and
   #:type-map-equal
   #:copy-type-map
   #:split-type-map
   #:impossible-type-map-p
   ))


(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:shadow #:compile-file)
  (:export
   #:*backend*
   #:compile-file
   #:type-inference
   #:trim-instruction-graph
   ))
