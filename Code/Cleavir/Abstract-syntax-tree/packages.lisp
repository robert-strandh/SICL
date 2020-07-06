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
   #:set-constant-symbol-value-ast #:make-set-constant-symbol-value-ast
   #:symbol #:symbol-ast
   #:fdefinition-ast #:make-fdefinition-ast #:info #:name-ast
   #:constant-fdefinition-ast #:make-constant-fdefinition-ast #:info
   #:constant-symbol-value-ast #:make-constant-symbol-value-ast #:info
   #:call-ast #:make-call-ast #:callee-ast #:argument-asts
   #:inline-declaration
   #:block-ast #:make-block-ast #:body
   #:function-ast #:make-function-ast #:lambda-list
   #:bound-declarations #:docstring #:original-lambda-list
   #:top-level-function-ast #:make-top-level-function-ast #:forms
   #:required-only-p #:required #:argparse-ast #:body-ast
   #:go-ast #:make-go-ast #:tag-ast
   #:if-ast #:make-if-ast #:test-ast #:then-ast #:else-ast
   #:branch-ast #:make-branch-ast #:branch-asts #:default-ast
   #:multiple-value-call-ast #:make-multiple-value-call-ast
   #:function-form-ast
   #:values-ast #:make-values-ast
   #:multiple-value-prog1-ast #:make-multiple-value-prog1-ast
   #:first-form-ast
   #:multiple-value-extract-ast #:make-multiple-value-extract-ast
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
   #:typew-ast #:make-typew-ast #:ctype
   #:the-typew-ast #:make-the-typew-ast
   #:bind-ast #:make-bind-ast
   #:eq-ast #:make-eq-ast #:neq-ast #:make-neq-ast
   #:case-ast #:make-case-ast #:comparees
   #:car-ast #:make-car-ast #:cons-ast
   #:cdr-ast #:make-cdr-ast
   #:rplaca-ast #:make-rplaca-ast #:object-ast
   #:rplacd-ast #:make-rplacd-ast
   #:coerce-ast #:from-type #:to-type #:arg-ast
   #:variable-ast #:operation-ast #:normal-ast #:overflow-ast
   #:fixnum-add-ast #:make-fixnum-add-ast
   #:fixnum-sub-ast #:make-fixnum-sub-ast
   #:arg1-ast #:arg2-ast #:variable-ast
   #:fixnum-less-ast #:make-fixnum-less-ast
   #:fixnum-not-greater-ast #:make-fixnum-not-greater-ast
   #:fixnum-greater-ast #:make-fixnum-greater-ast
   #:fixnum-not-less-ast #:make-fixnum-not-less-ast
   #:fixnum-equal-ast #:make-fixnum-equal-ast
   #:float-add-ast
   #:float-sub-ast
   #:float-mul-ast
   #:float-div-ast
   #:float-less-ast
   #:float-not-greater-ast
   #:float-greater-ast
   #:float-not-less-ast
   #:float-equal-ast
   #:float-sin-ast
   #:float-cos-ast
   #:float-sqrt-ast
   #:subtype
   #:slot-read-ast #:make-slot-read-ast #:slot-number-ast #:object-ast
   #:slot-write-ast #:make-slot-write-ast
   #:funcallable-slot-read-ast #:make-funcallable-slot-read-ast
   #:funcallable-slot-write-ast #:make-funcallable-slot-write-ast
   #:aref-ast #:aset-ast
   #:element-ast #:array-ast #:index-ast
   #:element-type #:simple-p #:boxed-p
   #:dynamic-allocation-ast #:make-dynamic-allocation-ast
   #:unreachable-ast #:make-unreachable-ast
   #:child-ast
   #:scope-ast #:make-scope-ast
   #:map-ast-depth-first-preorder
   ))

(defpackage #:cleavir-ast-graphviz
  (:use #:common-lisp #:cleavir-ast)
  (:shadowing-import-from #:cleavir-ast #:symbol)
  (:export
   #:draw-ast))
