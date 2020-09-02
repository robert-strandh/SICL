(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ir
  (:use #:common-lisp)
  (:export
   #:datum
   #:replace-datum
   #:immediate-input #:make-immediate-input
   #:load-time-value-input #:make-load-time-value-input
   #:form #:read-only-p
   #:word-input
   #:float-location #:make-float-location #:size
   #:constant-input #:make-constant-input
   #:value
   #:lexical-location #:make-lexical-location
   #:new-temporary
   #:name
   #:values-location #:make-values-location
   #:variable-p
   #:external-input #:make-external-input
   #:register-location
   #:layer #:index
   #:insert-instruction-before
   #:insert-instruction-between
   #:insert-instruction-after
   #:delete-instruction #:bypass-instruction
   #:reinitialize-data
   #:defining-instructions #:using-instructions
   #:instruction
   #:clone-instruction #:clone-initargs
   #:no-successors-mixin
   #:one-successor-mixin
   #:multiple-successors-mixin
   #:side-effect-mixin #:side-effect-free-p
   #:box-instruction #:unbox-instruction
   #:allocation-mixin #:dynamic-extent-p
   #:inputs #:outputs
   #:substitute-input #:substitute-output
   #:successors #:predecessors
   #:policy #:*policy*
   #:origin #:*origin*
   #:dynamic-environment #:*dynamic-environment*
   #:symbol-value-instruction #:make-symbol-value-instruction
   #:set-symbol-value-instruction #:make-set-symbol-value-instruction
   #:set-constant-symbol-value-instruction #:make-set-constant-symbol-value-instruction
   #:fdefinition-instruction #:make-fdefinition-instruction
   #:constant-fdefinition-instruction #:make-constant-fdefinition-instruction
   #:constant-symbol-value-instruction #:make-constant-symbol-value-instruction
   #:enter-instruction #:make-enter-instruction
   #:lambda-list #:bound-declarations #:closure-size #:docstring #:original-lambda-list
   #:static-environment #:dynamic-environment-output #:parameters
   #:top-level-enter-instruction #:make-top-level-enter-instruction #:forms
   #:nop-instruction #:make-nop-instruction
   #:unreachable-instruction #:make-unreachable-instruction
   #:assignment-instruction #:make-assignment-instruction
   #:funcall-instruction #:make-funcall-instruction
   #:inline-declaration #:attributes
   #:funcall-no-return-instruction #:make-funcall-no-return-instruction
   #:tailcall-instruction #:make-tailcall-instruction
   #:return-instruction #:make-return-instruction
   #:enclose-instruction #:make-enclose-instruction #:code #:initializer
   #:initialize-closure-instruction #:make-initialize-closure-instruction
   #:typeq-instruction #:make-typeq-instruction #:value-type
   #:the-instruction #:make-the-instruction
   #:typew-instruction #:make-typew-instruction #:ctype
   #:choke-instruction #:make-choke-instruction
   #:dynamic-allocation-instruction #:make-dynamic-allocation-instruction
   #:the-values-instruction #:make-the-values-instruction
   #:required-types #:optional-types #:rest-type
   #:save-values-instruction #:load-values-instruction
   #:catch-instruction #:make-catch-instruction
   #:unwind-instruction #:make-unwind-instruction #:destination #:unwind-index
   #:local-unwind-instruction #:make-local-unwind-instruction
   #:eq-instruction #:make-eq-instruction
   #:case-instruction #:make-case-instruction #:comparees
   #:consp-instruction #:make-consp-instruction
   #:fixnump-instruction #:make-fixnump-instruction
   #:characterp-instruction #:make-characterp-instruction
   #:phi-instruction #:make-phi-instruction
   #:use-instruction #:make-use-instruction
   #:aref-instruction #:aset-instruction
   #:element-type #:simple-p #:boxed-p
   #:coerce-instruction #:from-type #:to-type #:make-coerce-instruction
   #:fixnum-add-instruction #:make-fixnum-add-instruction
   #:fixnum-sub-instruction #:make-fixnum-sub-instruction
   #:fixnum-less-instruction #:make-fixnum-less-instruction
   #:fixnum-not-greater-instruction #:make-fixnum-not-greater-instruction
   #:fixnum-equal-instruction #:make-fixnum-equal-instruction
   #:float-add-instruction #:make-float-add-instruction
   #:float-sub-instruction #:make-float-sub-instruction
   #:float-mul-instruction #:make-float-mul-instruction
   #:float-div-instruction #:make-float-div-instruction
   #:subtype
   #:float-less-instruction #:make-float-less-instruction
   #:float-not-greater-instruction #:make-float-not-greater-instruction
   #:float-equal-instruction #:make-float-equal-instruction
   #:float-sin-instruction #:make-float-sin-instruction
   #:float-cos-instruction #:make-float-cos-instruction
   #:float-sqrt-instruction #:make-float-sqrt-instruction
   #:car-instruction #:make-car-instruction
   #:cdr-instruction #:make-cdr-instruction
   #:rplaca-instruction #:make-rplaca-instruction
   #:rplacd-instruction #:make-rplacd-instruction
   #:slot-read-instruction #:make-slot-read-instruction
   #:slot-write-instruction #:make-slot-write-instruction
   #:funcallable-slot-read-instruction #:make-funcallable-slot-read-instruction
   #:funcallable-slot-write-instruction #:make-funcallable-slot-write-instruction
   #:memref1-instruction #:make-memref1-instruction
   #:memref2-instruction #:make-memref2-instruction
   #:memset1-instruction #:make-memset1-instruction
   #:memset2-instruction #:make-memset2-instruction
   #:signed-add-instruction #:make-signed-add-instruction
   #:signed-sub-instruction #:make-signed-sub-instruction
   #:signed-less-instruction #:make-signed-less-instruction
   #:signed-not-greater-instruction #:make-signed-not-greater-instruction
   #:unsigned-add-instruction #:make-unsigned-add-instruction
   #:unsigned-sub-instruction #:make-unsigned-sub-instruction
   #:unsigned-less-instruction #:make-unsigned-less-instruction 
   #:unsigned-not-greater-instruction #:make-unsigned-not-greater-instruction
   #:equal-instruction #:make-equal-instruction
   #:multiple-to-fixed-instruction #:make-multiple-to-fixed-instruction
   #:fixed-to-multiple-instruction #:make-fixed-to-multiple-instruction
   #:multiple-value-call-instruction #:make-multiple-value-call-instruction
   #:create-cell-instruction #:make-create-cell-instruction
   #:fetch-instruction #:make-fetch-instruction
   #:read-cell-instruction #:make-read-cell-instruction 
   #:write-cell-instruction #:make-write-cell-instruction 
   #:add-activation-record-instruction
   #:make-add-activation-record-instruction
   #:remove-activation-record-instruction
   #:make-remove-activation-record-instruction
   #:load-from-static-environment-instruction
   #:make-load-from-static-environment-instruction
   #:store-to-static-environment-instruction
   #:make-store-to-static-environment-instruction
   #:map-instructions-arbitrary-order
   #:filter-instructions #:instructions-of-type
   #:map-instructions #:map-instructions-with-owner
   #:map-local-instructions #:filter-local-instructions
   #:local-instructions-of-type
   #:set-predecessors
   #:offset))

(defpackage #:cleavir-ir-graphviz
  (:use #:common-lisp #:cleavir-ir)
  (:export
   #:draw-instruction
   #:draw-datum
   #:draw-flowchart
   #:label
   #:input-label #:output-label
   #:*input-label-hook* #:*output-label-hook*
   #:name
   #:datum-id))
