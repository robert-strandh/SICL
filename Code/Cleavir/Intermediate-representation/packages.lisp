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
   #:enclose-instruction #:code #:initializer
   #:initialize-closure-instruction
   #:typeq-instruction #:value-type
   #:the-instruction
   #:dynamic-allocation-instruction
   #:the-values-instruction
   #:required-types #:optional-types #:rest-type
   #:catch-instruction
   #:unwind-instruction #:destination #:unwind-index
   #:bind-instruction
   #:unwind-protect-instruction
   #:eq-instruction
   #:consp-instruction
   #:fixnump-instruction
   #:characterp-instruction
   #:char-code-instruction
   #:code-char-instruction
   #:short-float-p-instruction
   #:single-float-p-instruction
   #:standard-object-p-instruction
   #:standard-object-class-of-instruction
   #:phi-instruction
   #:use-instruction
   #:load-constant-instruction
   #:location-info
   #:aref-instruction #:aset-instruction
   #:element-type #:simple-p #:boxed-p
   #:coerce-instruction #:from-type #:to-type
   #:fixnum-add-instruction
   #:fixnum-sub-instruction
   #:fixnum-less-instruction
   #:fixnum-not-greater-instruction
   #:fixnum-equal-instruction
   #:fixnum-multiply-instruction
   #:fixnum-divide-instruction
   #:fixnum-logand-instruction
   #:fixnum-logior-instruction
   #:fixnum-logxor-instruction
   #:fixnum-lognot-instruction
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
   #:nook-read-instruction
   #:nook-write-instruction
   #:multiple-to-fixed-instruction
   #:fixed-to-multiple-instruction
   #:save-values-instruction
   #:restore-values-instruction
   #:multiple-value-call-instruction
   #:initialize-values-instruction
   #:append-values-instruction
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
   #:offset
   #:compute-argument-count-instruction
   #:argument-instruction
   #:compute-return-value-count-instruction
   #:return-value-instruction
   #:initialize-return-values-instruction
   #:set-return-value-instruction
   ;; MIR instructions
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
   #:unsigned-div-instruction
   #:unsigned-less-instruction
   #:unsigned-not-greater-instruction
   #:negate-instruction
   #:equal-instruction
   #:shift-left-instruction
   #:logic-shift-right-instruction
   #:arithmetic-shift-right-instruction
   #:shifted-input #:shift-count
   #:bitwise-and-instruction
   #:bitwise-or-instruction
   #:bitwise-exclusive-or-instruction
   #:bitwise-not-instruction
   #:sign-extend-instruction
   ;; MIR data
   #:raw-datum
   #:raw-integer
   #:raw-float))
