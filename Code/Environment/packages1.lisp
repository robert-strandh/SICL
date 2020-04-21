(cl:in-package #:common-lisp-user)

(defpackage sicl-global-environment
  (:use #:common-lisp)
  (:shadow #:type)
  (:export
   #:global-environment
   #:definition
   #:location
   #:global-location #:make-global-location
   #:special-location #:make-special-location
   #:storage
   #:name
   #:type
   #:global-environment
   #:add-to-global-environment
   #:make-entry-from-declaration
   #:find-variable
   #:find-function
   #:find-type
   #:find-ftype
   #:augment-environment
   #:augment-environment-with-declarations
   #:add-lexical-variable-entry
   #:add-symbol-macro-entry
   #:find-function-cell
   #:special-operator-p
   #:symbol-macro-function
   #:find-value-cell
   #:ensure-function-entry))

