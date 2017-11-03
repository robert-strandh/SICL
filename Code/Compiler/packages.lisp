(defpackage #:sicl-compiler-phase-1
  (:nicknames #:p1)
  (:use #:common-lisp)
  (:shadow #:type
   )
  (:export
   #:ast
   #:*compile-file*
   #:convert-initial
   #:convert-top-level-form
   #:convert-top-level-lamda-expression
   #:convert-for-inlining
   #:convert #:convert-compound
   ))

(defpackage #:sicl-mir
  (:use #:common-lisp)
  (:export
   #:datum #:defining-instructions #:using-instructions
   #:immediate-input #:make-immediate-input
   #:word-input #:make-word-input
   #:constant-input #:make-constant-input
   #:lexical-location #:make-lexical-location #:name
   #:new-temporary
   #:special-location #:make-special-location #:storage
   #:global-input #:make-global-input
   #:load-time-input #:make-load-time-input #:initial-instruction
   #:external-input #:make-external-input #:value
   #:register-location #:make-register-location
   #:dynamic-location #:make-dynamic-location
   #:linkage-location #:make-linkage-location
   #:instruction #:predecessors #:successors #:inputs #:outputs
   #:insert-instruction-before #:insert-instruction-after
   #:insert-instruction-between #:delete-instruction
   #:reinitialize-data
   #:unique-id
   #:clone-instruction
   #:enter-instruction #:make-enter-instruction
   #:nop-instruction #:make-nop-instruction
   #:assignment-instruction #:make-assignment-instruction
   #:funcall-instruction #:make-funcall-instruction #:fun
   #:tailcall-instruction #:make-tailcall-instruction
   #:get-values-instruction #:make-get-values-instruction
   #:return-instruction #:make-return-instruction
   #:enclose-instruction #:make-enclose-instruction #:code
   #:get-argcount-instruction #:make-get-argcount-instruction
   #:get-arg-instruction #:make-get-arg-instruction
   #:load-constant-instruction #:make-load-constant-instruction 
   #:load-global-instruction #:make-load-global-instruction 
   #:linkage-vector-index
   #:load-static-env-instruction #:make-load-static-env-instruction 
   #:load-linkage-vector-instruction #:make-load-linkage-vector-instruction 
   #:memref-instruction #:make-memref-instruction #:cacheable #:displacement
   #:memset-instruction #:make-memset-instruction
   #:typeq-instruction #:make-typeq-instruction #:value-type
   #:u+-instruction #:make-u+-instruction
   #:u--instruction #:make-u--instruction
   #:s+-instruction #:make-s+-instruction
   #:s--instruction #:make-s--instruction
   #:neg-instruction #:make-neg-instruction
   #:&-instruction #:make-&-instruction
   #:ior-instruction #:make-ior-instruction
   #:xor-instruction #:make-xor-instruction
   #:~-instruction #:make-~-instruction
   #:==-instruction #:make-==-instruction
   #:s<-instruction #:make-s<-instruction
   #:s<=-instruction #:make-s<=-instruction
   #:u<-instruction #:make-u<-instruction
   #:u<=-instruction #:make-u<=-instruction
   #:catch-instruction #:make-catch-instruction
   #:unwind-instruction #:make-unwind-instruction
   #:draw-flowchart))

(defpackage #:sicl-compiler-phase-2
  (:nicknames #:p2)
  (:use #:common-lisp)
  (:export
   #:context #:results #:false-required-p
   #:nil-fill
   #:compile-ast
   #:compile-toplevel
   ))

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
