(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-lexical
  (:use #:common-lisp)
  (:shadowing-import-from
   ;; Import from this package:
   #:trucler
   ;; Query class accessors.
   #:name
   #:identity
   #:type
   #:inline
   #:ignore
   #:dynamic-extent
   #:compiler-macro
   #:expander
   #:value
   #:expansion
   ;; Augmentation functions.
   #:add-lexical-variable
   #:add-special-variable
   #:add-local-symbol-macro
   #:add-local-function
   #:add-local-macro
   #:add-block
   #:add-tag
   #:add-variable-type
   #:add-function-type
   #:add-variable-ignore
   #:add-function-ignore
   #:add-variable-dynamic-extent
   #:add-function-dynamic-extent
   #:add-inline
   #:add-speed
   #:add-compilation-speed
   #:add-debug
   #:add-safety
   #:add-space)
  (:export
   ;; The environment class.
   #:environment
   ;; Query class accessors.
   #:name
   #:identity
   #:type
   #:inline
   #:ignore
   #:dynamic-extent
   #:compiler-macro
   #:expander
   #:value
   #:expansion
   ;; Augmentation functions.
   #:add-lexical-variable
   #:add-special-variable
   #:add-local-symbol-macro
   #:add-local-function
   #:add-local-macro
   #:add-block
   #:add-tag
   #:add-variable-type
   #:add-function-type
   #:add-variable-ignore
   #:add-function-ignore
   #:add-variable-dynamic-extent
   #:add-function-dynamic-extent
   #:add-inline
   #:add-speed
   #:add-compilation-speed
   #:add-debug
   #:add-safety
   #:add-space))
