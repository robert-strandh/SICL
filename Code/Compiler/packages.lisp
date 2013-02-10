(defpackage #:sicl-compiler-environment
  (:nicknames #:sicl-env)
  (:use #:common-lisp)
  (:shadow #:type
	   #:proclaim
	   #:macroexpand-1
	   #:macroexpand
	   #:macro-function
	   #:*macroexpand-hook*
	   )
  (:export
   #:definition
   #:location #:lexical-location #:global-location #:special-location
   #:storage
   #:name
   #:type
   #:*global-environment*
   #:add-to-global-environment
   #:make-entry-from-declaration
   #:find-variable
   #:find-function
   #:find-block
   #:find-go-tag
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
   #:add-macro-entry
   #:add-block-entry
   #:add-go-tag-entry
   #:constant-variable-info
   #:macro-info
   #:symbol-macro-info
   #:block-info
   #:tag-info
   #:special-location-info
   #:lexical-location-info
   #:global-location-info
   #:function-info
   #:variable-info
   ))

(defpackage #:sicl-compiler-phase-1
  (:nicknames #:p1)
  (:use #:common-lisp)
  (:shadow #:type
   )
  (:export
   #:ast
   #:constant-ast #:value
   #:typed-location-ast #:location #:type
   #:function-call-ast #:function-location #:arguments
   #:block-ast #:body
   #:catch-ast #:tag 
   #:eval-when-ast #:situations
   #:function-ast #:body-ast
   #:go-ast #:tag-ast
   #:if-ast #:test #:then #:else
   #:load-time-value-ast #:read-only-p
   #:multiple-value-call-ast #:function-ast #:argument-asts
   #:multiple-value-prog1-ast #:first-ast #:body-asts
   #:progn-ast #:form-asts
   #:progv-ast #:symbols-ast #:vals-ast
   #:return-from-ast #:form-ast
   #:setq-ast #:location-ast #:value-ast
   #:tagbody-ast #:items
   #:tag-ast #:name
   #:the-ast #:value-type
   #:throw-ast
   #:unwind-protect-ast #:protected-ast #:cleanup-form-asts
   #:lambda-list #:required #:optional #:rest-body
   #:keys #:allow-other-keys #:aux
   #:convert #:convert-compound
   #:draw-ast #:id
   ))

(defpackage #:sicl-compiler-phase-2
  (:nicknames #:p2)
  (:use #:common-lisp)
  (:export
   #:context #:results #:false-required-p
   #:instruction #:successors #:inputs #:outputs
   #:nil-fill
   #:end-instruction
   #:nop-instruction
   #:constant-assignment-instruction #:constant
   #:variable-assignment-instruction
   #:test-instruction #:test
   #:funcall-instruction #:fun
   #:get-arguments-instruction #:lambda-list
   #:get-values-instruction
   #:put-arguments-instruction
   #:put-values-instruction
   #:enter-instruction
   #:leave-instruction
   #:return-instruction
   #:enclose-instruction #:code
   #:go-instruction
   #:compile-ast
   #:compile-toplevel
   #:new-temporary
   #:draw-instruction
   #:draw-flowchart
   #:id
   #:*instruction-table*
   ))

(defpackage #:sicl-word
  (:use #:common-lisp)
  (:export
   #:memalloc #:memref #:memset
   #:u+ #:u- #:s+ #:s- #:neg
   #:u* #:s*
   #:lshift #:ashift
   #:& #:ior #:xor #:~
   #:== #:s< #:s<= #:u< #:u<=
   ))

