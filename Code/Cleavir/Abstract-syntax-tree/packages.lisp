(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ast
  (:use #:common-lisp)
  (:shadow #:symbol)
  (:export
   #:ast #:children
   #:source-info
   #:origin
   #:*policy* #:policy
   #:boolean-ast-mixin
   #:no-value-ast-mixin
   #:one-value-ast-mixin
   #:side-effect-free-ast-mixin
   #:side-effect-free-p
   #:immediate-ast #:make-immediate-ast #:value
   #:constant-ast #:make-constant-ast #:value
   #:lexical-ast #:make-lexical-ast
   #:symbol-value-ast #:make-symbol-value-ast
   #:set-symbol-value-ast #:make-set-symbol-value-ast
   #:symbol #:symbol-ast
   #:fdefinition-ast #:make-fdefinition-ast #:info #:name-ast
   #:call-ast #:make-call-ast #:callee-ast #:argument-asts
   #:block-ast #:make-block-ast #:body
   #:function-ast #:make-function-ast #:lambda-list
   #:top-level-function-ast #:make-top-level-function-ast #:forms
   #:required-only-p #:required #:argparse-ast #:body-ast
   #:go-ast #:make-go-ast #:tag-ast
   #:if-ast #:make-if-ast #:test-ast #:then-ast #:else-ast
   #:multiple-value-call-ast #:make-multiple-value-call-ast
   #:function-form-ast
   #:values-ast #:make-values-ast
   #:multiple-value-prog1-ast #:make-multiple-value-prog1-ast
   #:first-form-ast
   #:load-time-value-ast #:make-load-time-value-ast #:read-only-p
   #:form
   #:body-asts
   #:progn-ast #:make-progn-ast #:form-asts
   #:return-from-ast #:make-return-from-ast #:form-ast
   #:setq-ast #:make-setq-ast #:lhs-ast #:value-ast
   #:multiple-value-setq-ast #:make-multiple-value-setq-ast #:lhs-asts
   #:tagbody-ast #:make-tagbody-ast #:item-asts
   #:tag-ast #:make-tag-ast #:name
   #:the-ast #:make-the-ast
   #:required-types #:optional-types #:rest-type
   #:typeq-ast #:make-typeq-ast #:type-specifier #:type-specifier-ast
   #:bind-ast #:make-bind-ast
   #:eq-ast #:make-eq-ast
   #:car-ast #:make-car-ast #:cons-ast
   #:cdr-ast #:make-cdr-ast
   #:rplaca-ast #:make-rplaca-ast #:object-ast
   #:rplacd-ast #:make-rplacd-ast
   #:variable-ast #:operation-ast #:normal-ast #:overflow-ast
   #:fixnum-add-ast #:make-fixnum-add-ast
   #:fixnum-sub-ast #:make-fixnum-sub-ast
   #:arg1-ast #:arg2-ast #:variable-ast
   #:fixnum-less-ast #:make-fixnum-less-ast
   #:fixnum-not-greater-ast #:make-fixnum-not-greater-ast
   #:fixnum-greater-ast #:make-fixnum-greater-ast
   #:fixnum-not-less-ast #:make-fixnum-not-less-ast
   #:fixnum-equal-ast #:make-fixnum-equal-ast
   #:short-float-add-ast
   #:short-float-sub-ast
   #:short-float-mul-ast
   #:short-float-div-ast
   #:short-float-less-ast
   #:short-float-not-greater-ast
   #:short-float-greater-ast
   #:short-float-not-less-ast
   #:short-float-equal-ast
   #:short-float-sin-ast
   #:short-float-cos-ast
   #:short-float-sqrt-ast
   #:single-float-add-ast
   #:single-float-sub-ast
   #:single-float-mul-ast
   #:single-float-div-ast
   #:single-float-less-ast
   #:single-float-not-greater-ast
   #:single-float-greater-ast
   #:single-float-not-less-ast
   #:single-float-equal-ast
   #:single-float-sin-ast
   #:single-float-cos-ast
   #:single-float-sqrt-ast
   #:double-float-add-ast
   #:double-float-sub-ast
   #:double-float-mul-ast
   #:double-float-div-ast
   #:double-float-less-ast
   #:double-float-not-greater-ast
   #:double-float-greater-ast
   #:double-float-not-less-ast
   #:double-float-equal-ast
   #:double-float-sin-ast
   #:double-float-cos-ast
   #:double-float-sqrt-ast
   #:long-float-add-ast
   #:long-float-sub-ast
   #:long-float-mul-ast
   #:long-float-div-ast
   #:long-float-less-ast
   #:long-float-not-greater-ast
   #:long-float-greater-ast
   #:long-float-not-less-ast
   #:long-float-equal-ast
   #:long-float-sin-ast
   #:long-float-cos-ast
   #:long-float-sqrt-ast
   #:slot-read-ast #:make-slot-read-ast #:slot-number-ast #:object-ast
   #:slot-write-ast #:make-slot-write-ast
   #:aref-ast #:aset-ast
   #:element-ast #:array-ast #:index-ast
   #:element-type #:simple-p #:boxed-p
   #:child-ast
   #:scope-ast #:make-scope-ast
   #:map-ast-depth-first-preorder
   ))

(defpackage #:cleavir-ast-graphviz
  (:use #:common-lisp #:cleavir-ast)
  (:shadowing-import-from #:cleavir-ast #:symbol)
  (:export
   #:draw-ast))
