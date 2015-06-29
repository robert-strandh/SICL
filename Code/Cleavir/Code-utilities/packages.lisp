(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-code-utilities
  (:use #:common-lisp)
  (:export #:list-structure
	   #:proper-list-p
	   #:proper-list-length
	   #:dotted-list-p
	   #:dotted-list-length
	   #:proper-or-dotted-list-length
	   #:form-must-be-proper-list
	   #:check-form-proper-list
	   #:invalid-number-of-arguments
	   #:check-argcount
	   #:circular-list-p
	   #:lambda-list
	   #:required
	   #:environment
	   #:whole
	   #:optionals
	   #:rest-body
	   #:keys
	   #:allow-other-keys
	   #:aux
	   #:parse-ordinary-lambda-list
	   #:parse-generic-function-lambda-list
	   #:parse-specialized-lambda-list
	   #:parse-macro-lambda-list
	   #:parse-destructuring-lambda-list
	   #:preprocess-lambda-list
	   #:parse-deftype-lambda-list
	   #:parse-defsetf-lambda-list
	   #:parse-define-modify-macro-lambda-list
	   #:parse-define-method-combination-arguments-lambda-list
	   #:lambda-list-variables
	   #:destructure-lambda-list
	   #:match-lambda-list
	   #:parse-macro
	   #:parse-compiler-macro
	   #:parse-deftype
	   #:lambda-lists-congruent-p
	   #:generate-congruent-lambda-list
	   #:lambda-list-type-specifier
	   #:canonicalize-declaration-specifiers
	   #:separate-ordinary-body
	   #:separate-function-body))

