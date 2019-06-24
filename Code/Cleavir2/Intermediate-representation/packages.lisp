(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ir
  (:use #:common-lisp)
  (:export
   #:datum
   #:replace-datum
   #:immediate-input #:make-immediate-input
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
   ;; FIXME: check that these two are still needed.
   #:dynamic-environment #:*dynamic-environment*
   #:dynamic-environment-location
   #:symbol-value-instruction
   #:set-symbol-value-instruction
   #:fdefinition-instruction
   #:enter-instruction #:make-enter-instruction
   #:lambda-list #:closure-size
   #:static-environment #:dynamic-environment-output #:parameters
   #:top-level-enter-instruction #:make-top-level-enter-instruction #:forms
   #:nop-instruction
   #:unreachable-instruction
   #:assignment-instruction
   #:funcall-instruction
   #:funcall-no-return-instruction
   #:tailcall-instruction
   #:return-instruction
   #:enclose-instruction #:code
   #:typeq-instruction #:value-type
   #:the-instruction
   #:dynamic-allocation-instruction
   #:the-values-instruction
   #:required-types #:optional-types #:rest-type
   #:catch-instruction
   #:unwind-instruction #:destination #:unwind-index
   #:bind-instruction
   #:eq-instruction
   #:consp-instruction
   #:fixnump-instruction
   #:characterp-instruction
   #:phi-instruction
   #:use-instruction
   #:aref-instruction #:aset-instruction
   #:element-type #:simple-p #:boxed-p
   #:coerce-instruction #:from-type #:to-type
   #:fixnum-add-instruction
   #:fixnum-sub-instruction
   #:fixnum-less-instruction
   #:fixnum-not-greater-instruction
   #:fixnum-equal-instruction
   #:float-add-instruction
   #:float-sub-instruction
   #:float-mul-instruction
   #:float-div-instruction
   #:subtype
   #:float-less-instruction
   #:float-not-greater-instruction
   #:float-equal-instruction
   #:float-sin-instruction
   #:float-cos-instruction
   #:float-sqrt-instruction
   #:car-instruction
   #:cdr-instruction
   #:rplaca-instruction
   #:rplacd-instruction
   #:slot-read-instruction
   #:slot-write-instruction
   #:memref1-instruction
   #:memref2-instruction
   #:memset1-instruction
   #:memset2-instruction
   #:signed-add-instruction
   #:signed-sub-instruction
   #:signed-less-instruction
   #:signed-not-greater-instruction
   #:unsigned-add-instruction
   #:unsigned-sub-instruction
   #:unsigned-less-instruction
   #:unsigned-not-greater-instruction
   #:equal-instruction
   #:multiple-to-fixed-instruction
   #:fixed-to-multiple-instruction
   #:multiple-value-call-instruction
   #:create-cell-instruction
   #:fetch-instruction
   #:read-cell-instruction
   #:write-cell-instruction
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
