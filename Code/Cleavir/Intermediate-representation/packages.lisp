(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ir
  (:use #:common-lisp)
  (:export
   #:datum
   #:immediate-input #:make-immediate-input
   #:load-time-value-input #:make-load-time-value-input
   #:form #:read-only-p
   #:word-input
   #:float-location #:make-float-location #:size
   #:constant-input #:make-constant-input
   #:value
   #:lexical-location #:make-lexical-location
   #:new-temporary
   #:dynamic-lexical-location #:make-dynamic-lexical-location
   #:new-dynamic-temporary
   #:static-lexical-location #:make-static-lexical-location
   #:name
   #:values-location #:make-values-location
   #:external-input #:make-external-input
   #:register-location
   #:dynamic-location
   #:layer #:index
   #:insert-instruction-before
   #:insert-instruction-between
   #:insert-instruction-after
   #:delete-instruction
   #:reinitialize-data
   #:defining-instructions #:using-instructions
   #:instruction
   #:no-successors-mixin
   #:one-successor-mixin
   #:two-successors-mixin
   #:box-instruction-mixin #:box-instruction-p
   #:unbox-instruction-mixin #:unbox-instruction-p
   #:side-effect-free-mixin #:side-effect-free-p
   #:inputs #:outputs
   #:successors #:predecessors
   #:symbol-value-instruction #:make-symbol-value-instruction
   #:set-symbol-value-instruction #:make-set-symbol-value-instruction
   #:fdefinition-instruction #:make-fdefinition-instruction
   #:enter-instruction #:make-enter-instruction #:lambda-list
   #:top-level-enter-instruction #:make-top-level-enter-instruction #:forms
   #:nop-instruction #:make-nop-instruction
   #:assignment-instruction #:make-assignment-instruction
   #:funcall-instruction #:make-funcall-instruction
   #:tailcall-instruction #:make-tailcall-instruction
   #:return-instruction #:make-return-instruction
   #:enclose-instruction #:make-enclose-instruction #:code
   #:typeq-instruction #:make-typeq-instruction #:value-type
   #:the-instruction #:make-the-instruction
   #:catch-instruction #:make-catch-instruction
   #:unwind-instruction #:make-unwind-instruction #:invocation
   #:eq-instruction #:make-eq-instruction
   #:consp-instruction #:make-consp-instruction
   #:fixnump-instruction #:make-fixnump-instruction
   #:phi-instruction #:make-phi-instruction
   #:fixnum-add-instruction #:make-fixnum-add-instruction
   #:fixnum-sub-instruction #:make-fixnum-sub-instruction
   #:fixnum-less-instruction #:make-fixnum-less-instruction
   #:fixnum-not-greater-instruction #:make-fixnum-not-greater-instruction
   #:fixnum-equal-instruction #:make-fixnum-equal-instruction
   #:bit-unbox-instruction #:make-bit-unbox-instruction
   #:bit-box-instruction #:make-bit-box-instruction
   #:unsigned-byte-8-unbox-instruction #:make-unsigned-byte-8-unbox-instruction
   #:unsigned-byte-8-box-instruction #:make-unsigned-byte-8-box-instruction
   #:short-float-unbox-instruction #:make-short-float-unbox-instruction 
   #:short-float-box-instruction #:make-short-float-box-instruction 
   #:short-float-add-instruction #:make-short-float-add-instruction 
   #:short-float-sub-instruction #:make-short-float-sub-instruction 
   #:short-float-mul-instruction #:make-short-float-mul-instruction 
   #:short-float-div-instruction #:make-short-float-div-instruction 
   #:short-float-sin-instruction #:make-short-float-sin-instruction 
   #:short-float-cos-instruction #:make-short-float-cos-instruction 
   #:short-float-sqrt-instruction #:make-short-float-sqrt-instruction 
   #:short-float-less-instruction
   #:short-float-not-greater-instruction
   #:short-float-equal-instruction
   #:single-float-unbox-instruction #:make-single-float-unbox-instruction 
   #:single-float-box-instruction #:make-single-float-box-instruction 
   #:single-float-add-instruction #:make-single-float-add-instruction 
   #:single-float-sub-instruction #:make-single-float-sub-instruction 
   #:single-float-mul-instruction #:make-single-float-mul-instruction 
   #:single-float-div-instruction #:make-single-float-div-instruction 
   #:single-float-sin-instruction #:make-single-float-sin-instruction 
   #:single-float-cos-instruction #:make-single-float-cos-instruction 
   #:single-float-sqrt-instruction #:make-single-float-sqrt-instruction 
   #:single-float-less-instruction
   #:single-float-not-greater-instruction
   #:single-float-equal-instruction
   #:double-float-unbox-instruction #:make-double-float-unbox-instruction 
   #:double-float-box-instruction #:make-double-float-box-instruction 
   #:double-float-add-instruction #:make-double-float-add-instruction 
   #:double-float-sub-instruction #:make-double-float-sub-instruction 
   #:double-float-mul-instruction #:make-double-float-mul-instruction 
   #:double-float-div-instruction #:make-double-float-div-instruction 
   #:double-float-sin-instruction #:make-double-float-sin-instruction 
   #:double-float-cos-instruction #:make-double-float-cos-instruction 
   #:double-float-sqrt-instruction #:make-double-float-sqrt-instruction 
   #:double-float-less-instruction
   #:double-float-not-greater-instruction
   #:double-float-equal-instruction
   #:long-float-unbox-instruction #:make-long-float-unbox-instruction 
   #:long-float-box-instruction #:make-long-float-box-instruction 
   #:long-float-add-instruction #:make-long-float-add-instruction 
   #:long-float-sub-instruction #:make-long-float-sub-instruction 
   #:long-float-mul-instruction #:make-long-float-mul-instruction 
   #:long-float-div-instruction #:make-long-float-div-instruction 
   #:long-float-sin-instruction #:make-long-float-sin-instruction 
   #:long-float-cos-instruction #:make-long-float-cos-instruction 
   #:long-float-sqrt-instruction #:make-long-float-sqrt-instruction 
   #:long-float-less-instruction
   #:long-float-not-greater-instruction
   #:long-float-equal-instruction
   #:unboxed-integer-to-unboxed-short-float-instruction
   #:make-unboxed-integer-to-unboxed-short-float-instruction
   #:unboxed-integer-to-unboxed-single-float-instruction
   #:make-unboxed-integer-to-unboxed-single-float-instruction
   #:unboxed-integer-to-unboxed-double-float-instruction
   #:make-unboxed-integer-to-unboxed-double-float-instruction
   #:unboxed-integer-to-unboxed-long-float-instruction
   #:make-unboxed-integer-to-unboxed-long-float-instruction
   #:car-instruction #:make-car-instruction
   #:cdr-instruction #:make-cdr-instruction
   #:rplaca-instruction #:make-rplaca-instruction
   #:rplacd-instruction #:make-rplacd-instruction
   #:slot-read-instruction #:make-slot-read-instruction
   #:slot-write-instruction #:make-slot-write-instruction
   #:t-aref-instruction #:make-t-aref-instruction
   #:bit-aref-instruction #:make-bit-aref-instruction
   #:unsigned-byte-8-aref-instruction #:make-unsigned-byte-8-aref-instruction
   #:short-float-aref-instruction #:make-short-float-aref-instruction
   #:single-float-aref-instruction #:make-single-float-aref-instruction
   #:double-float-aref-instruction #:make-double-float-aref-instruction
   #:long-float-aref-instruction #:make-long-float-aref-instruction
   #:t-aset-instruction #:make-t-aset-instruction
   #:bit-aset-instruction #:make-bit-aset-instruction
   #:unsigned-byte-8-aset-instruction #:make-unsigned-byte-8-aset-instruction
   #:short-float-aset-instruction #:make-short-float-aset-instruction
   #:single-float-aset-instruction #:make-single-float-aset-instruction
   #:double-float-aset-instruction #:make-double-float-aset-instruction
   #:long-float-aset-instruction #:make-long-float-aset-instruction
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
   #:hir-to-mir #:specialize
   #:map-instructions-arbitrary-order
   #:map-instructions
   #:map-instructions-with-owner
   #:map-instructions-by-owner
   #:map-instructions-by/with-owner
   #:set-predecessors))

(defpackage #:cleavir-ir-graphviz
  (:use #:common-lisp #:cleavir-ir)
  (:export
   #:draw-instruction
   #:draw-datum
   #:draw-flowchart
   #:label
   #:name
   #:datum-id))
