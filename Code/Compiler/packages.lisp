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

(defpackage #:sicl-compiler-environment
  (:nicknames #:sicl-env)
  (:use #:common-lisp)
  (:shadow #:type
	   #:package
	   #:proclaim
	   #:macroexpand-1
	   #:macroexpand
	   #:macro-function
	   #:*macroexpand-hook*
	   #:fdefinition
	   #:fboundp
	   #:fmakunbound
	   #:special-operator-p
	   )
  (:export
   #:proclaim
   #:definition
   #:location
   #:lexical-location #:make-lexical-location
   #:global-location #:make-global-location
   #:special-location #:make-special-location
   #:storage
   #:name
   #:type
   #:*global-environment*
   #:add-to-global-environment
   #:make-entry-from-declaration
   #:find-variable
   #:find-function
   #:macroexpand-1
   #:macroexpand
   #:find-type
   #:find-ftype
   #:augment-environment
   #:augment-environment-with-declarations
   #:add-constant-variable-entry
   #:add-special-variable-entry
   #:add-lexical-variable-entry
   #:add-symbol-macro-entry
   #:add-global-function-entry
   #:add-local-function-entry
   #:add-local-macro-entry
   #:add-block-entry
   #:add-go-tag-entry
   #:constant-variable-info
   #:macro-info
   #:symbol-macro-info
   #:block-info
   #:tag-info
   #:location-info
   #:special-location-info
   #:lexical-location-info
   #:global-location-info
   #:function-info
   #:variable-info
   #:fdefinition
   #:fboundp
   #:fmakunbound
   #:special-operator-p
   ))

(defpackage #:sicl-ast
  (:use #:common-lisp)
  (:export
   #:ast
   #:immediate-ast #:make-immediate-ast #:value
   #:call-ast #:make-call-ast #:callee-ast #:argument-asts
   #:block-ast #:make-block-ast #:body
   #:eval-when-ast #:make-eval-when-ast #:situations
   #:function-ast #:make-function-ast #:body-ast
   #:go-ast #:make-go-ast #:tag-ast
   #:if-ast #:make-if-ast #:test-ast #:then-ast #:else-ast
   #:load-time-value-ast #:make-load-time-value-ast #:read-only-p
   #:lambda-list #:required
   #:body-asts
   #:progn-ast #:make-progn-ast #:form-asts
   #:return-from-ast #:make-return-from-ast #:form-ast
   #:setq-ast #:make-setq-ast #:lhs-ast #:value-ast
   #:tagbody-ast #:make-tagbody-ast #:items
   #:tag-ast #:make-tag-ast #:name
   #:the-ast #:make-the-ast #:value-type
   #:draw-ast
   #:children
   #:word-ast #:make-word-ast
   #:memref-ast #:make-memref-ast
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
   #:halt-ast #:make-halt-ast))

(defpackage #:sicl-compiler-phase-1
  (:nicknames #:p1)
  (:use #:common-lisp)
  (:shadow #:type
   )
  (:export
   #:ast
   #:convert-top-level-form
   #:convert #:convert-compound
   ))

(defpackage #:sicl-mir
  (:use #:common-lisp)
  (:export
   #:immediate-input #:make-immediate-input
   #:external-input #:make-external-input #:value
   #:instruction #:successors #:inputs #:outputs
   #:nop-instruction #:make-nop-instruction
   #:assignment-instruction #:make-assignment-instruction
   #:test-instruction #:make-test-instruction #:test
   #:funcall-instruction #:make-funcall-instruction #:fun
   #:get-arguments-instruction #:make-get-arguments-instruction #:lambda-list
   #:get-values-instruction #:make-get-values-instruction
   #:put-arguments-instruction #:make-put-arguments-instruction
   #:put-values-instruction #:make-put-values-instruction
   #:enter-instruction #:make-enter-instruction
   #:leave-instruction #:make-leave-instruction
   #:return-instruction #:make-return-instruction
   #:enclose-instruction #:make-enclose-instruction #:code
   #:go-instruction #:make-go-instruction
   #:memalloc-instruction #:make-memalloc-instruction
   #:memref-instruction #:make-memref-instruction
   #:memset-instruction #:make-memset-instruction
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
   #:new-temporary
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
   ;; doesn't strictly belong here
   #:find-function-cell
   ))

(defpackage #:sicl-procedure-integration
  (:use #:common-lisp)
  (:export #:integrate-procedures))

(defpackage #:externals-elimination
  (:use #:common-lisp)
  (:export #:eliminate-externals))

(defpackage #:sicl-program
  (:use #:common-lisp)
  (:export
   ))

(defpackage #:sicl-type-proclamations
  (:use #:common-lisp)
  (:shadowing-import-from #:sicl-compiler-environment
			  #:proclaim))
