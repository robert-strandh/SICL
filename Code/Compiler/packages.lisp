(defpackage #:sicl-configuration
  (:use #:common-lisp)
  (:export
   #:+word-size-in-bytes+
   #:+word-size-in-bits+
   #:+tag-fixnum+
   #:+tag-cons+
   #:+tag-immediate+
   #:+tag-other+
   #:+tag-mask+
   #:+tag-width+
   #:+immediate-tag-mask+
   #:+tag-character+
   #:+immediate-tag-width+
   #:+most-positive-fixnum+
   #:+most-negative-fixnum+
   #:host-integer-to-word
   #:host-char-to-word
   #:+unbound+
   #:+car-offset+
   #:+cdr-offset+
   #:+class-offset+
   #:+contents-offset+
   #:+mmap-base+
   #:+free+
   #:+global-environment+
   #:+function-memalloc+
   #:+function-find-package+
   #:+function-find-symbol+
   #:+function-find-class+
   #:+function-find-function-cell+
   #:+class-symbol+
   #:+class-package+
   #:+class-simple-vector+
   #:+class-simple-string+
   #:+class-octet-vector+
   #:+class-function+
   #:+class-code+
   #:+class-environment+
   #:+class-builtin-class+
   #:+symbol-nil+
   #:+symbol-t+
   #:+argument-count+
   #:+arguments+
   #:+heap-start+
   ))

(defpackage #:sicl-ast
  (:use #:common-lisp)
  (:export
   #:ast
   #:immediate-ast #:make-immediate-ast
   #:constant-ast #:make-constant-ast #:value
   #:global-ast #:make-global-ast #:storage #:function-type
   #:special-ast #:make-special-ast
   #:lexical-ast #:make-lexical-ast
   #:call-ast #:make-call-ast #:callee-ast #:argument-asts
   #:block-ast #:make-block-ast #:body
   #:function-ast #:make-function-ast
   #:required-only-p #:required #:argparse-ast #:body-ast
   #:go-ast #:make-go-ast #:tag-ast
   #:if-ast #:make-if-ast #:test-ast #:then-ast #:else-ast
   #:load-time-value-ast #:make-load-time-value-ast #:read-only-p
   #:body-asts
   #:progn-ast #:make-progn-ast #:form-asts
   #:return-from-ast #:make-return-from-ast #:form-ast
   #:setq-ast #:make-setq-ast #:lhs-ast #:value-ast
   #:tagbody-ast #:make-tagbody-ast #:items
   #:tag-ast #:make-tag-ast #:name
   #:the-ast #:make-the-ast #:type-specifiers
   #:draw-ast
   #:children
   #:typeq-ast #:make-typeq-ast #:type-specifier #:type-specifier-ast
   #:load-constant-ast #:make-load-constant-ast
   #:load-global-ast #:make-load-global-ast #:offset
   #:word-ast #:make-word-ast
   #:memref-ast #:make-memref-ast #:displacement
   #:memset-ast #:make-memset-ast
   #:u+-ast #:make-u+-ast
   #:u--ast #:make-u--ast
   #:s+-ast #:make-s+-ast
   #:s--ast #:make-s--ast
   #:neg-ast #:make-neg-ast
   #:u*-ast #:make-u*-ast
   #:s*-ast #:make-s*-ast
   #:lshift-ast #:make-lshift-ast
   #:ashift-ast #:make-ashift-ast
   #:&-ast #:make-&-ast
   #:ior-ast #:make-ior-ast
   #:xor-ast #:make-xor-ast
   #:~-ast #:make-~-ast
   #:==-ast #:make-==-ast
   #:s<-ast #:make-s<-ast
   #:s<=-ast #:make-s<=-ast
   #:u<-ast #:make-u<-ast
   #:u<=-ast #:make-u<=-ast
   #:argcount-ast #:make-argcount-ast
   #:arg-ast #:make-arg-ast #:index-ast
   #:halt-ast #:make-halt-ast))

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
