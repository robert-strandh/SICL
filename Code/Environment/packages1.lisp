(cl:in-package #:common-lisp-user)

(defpackage sicl-global-environment
  (:use #:common-lisp)
  (:shadow #:type)
  (:export
   #:definition
   #:location
   #:lexical-location #:make-lexical-location
   #:global-location #:make-global-location
   #:special-location #:make-special-location
   #:storage
   #:name
   #:type
   #:global-environment
   #:*global-environment*
   #:add-to-global-environment
   #:make-entry-from-declaration
   #:find-variable
   #:find-function
   #:fully-expand-form
   #:find-type
   #:find-ftype
   #:augment-environment
   #:augment-environment-with-declarations
   #:add-lexical-variable-entry
   #:add-symbol-macro-entry
   #:add-local-function-entry
   #:add-local-macro-entry
   #:add-block-entry
   #:add-go-tag-entry
   #:constant-variable-info
   #:macro-info
   #:symbol-macro-info
   #:block-info
   #:tag-info
   #:location-info
   #:special-location-info
   #:global-location-info
   #:function-info
   #:inline-info #:ast #:parameters
   #:variable-info
   #:find-function-cell
   #:special-operator-p
   #:symbol-macro-function
   #:find-value-cell
   #:ensure-global-function-entry))

