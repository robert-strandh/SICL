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
   #:constant-variable-entry #:make-constant-variable-entry
   #:special-variable-entry #:make-special-variable-entry
   #:lexical-variable-entry #:make-lexical-variable-entry
   #:symbol-macro-entry #:make-symbol-macro-entry
   #:global-function-entry #:make-global-function-entry
   #:local-function-entry #:make-local-function-entry
   #:macro-entry #:make-macro-entry
   #:block-entry #:make-block-entry
   #:go-tag-entry #:make-go-tag-entry
   #:type-declaration-entry #:make-type-declaration-entry
   #:ftype-declaration-entry #:make-ftype-declaration-entry
   #:inline-declaration-entry #:make-inline-declaration-entry
   #:notinline-declaration-entry #:make-notinline-declaration-entry
   #:dynamic-extent-declaration-entry #:make-dynamic-extent-declaration-entry
   #:ignore-decalration-entry #:make-ignore-decalration-entry
   #:ignorable-declaration-entry #:make-ignorable-declaration-entry
   #:optimize-declaration-entry #:make-optimize-declaration-entry
   #:declaration-declaration-entry #:make-declaration-declaration-entry
   #:definition
   #:location #:lexical-location
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
   #:find-fype  ; remove later
   #:augment-environment
   #:augment-environment-with-declarations
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
   #:close-ast #:code-ast #:body-ast
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
   #:instruction #:successors #:inputs #:outputs
   #:end-instruction
   #:nop-instruction
   #:constant-assignment-instruction #:constant
   #:variable-assignment-instruction
   #:test-instruction #:test
   #:funcall-instruction #:fun
   #:enter-instruction #:lambda-list
   #:leave-instruction
   #:return-instruction
   #:close-instruction
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

