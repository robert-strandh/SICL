(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadow #:get-setf-expansion type)
  (:shadowing-import-from
   #:clostrum
   .
   #.(loop for symbol being each external-symbol in '#:clostrum
           unless (member symbol '(clostrum:run-time-environment
                                   clostrum:compilation-environment
                                   clostrum:function-description
                                   clostrum:class-description
                                   clostrum:variable-description))
             collect (symbol-name symbol)))
  (:import-from #:clostrum-basic
                #:function-cell
                #:variable-cell)
  (:export #:global-environment
           #:client
           #:method-combination-template
           #:base-run-time-environment
           #:run-time-environment
           #:evaluation-environment
           #:compilation-environment
           #:function-description
           #:simple-function-description
           #:generic-function-description
           #:lambda-list
           #:class-name
           #:method-class-name
           #:method-combination-info
           #:get-setf-expansion
           #:variable-description
           #:constant-variable-description
           #:special-variable-description
           #:class-description
           #:name
           #:superclass-names
           #:metaclass-name
           #:type
           #:value
           #:find-method-combination-template
           #:structure-description
           #:function-cell
           #:variable-cell
           #:traced-functions
           #:map-defined-functions
           #:map-defined-classes
           #:map-defined-method-combination-templates
           .
           #.(loop for symbol being each external-symbol in '#:clostrum
                   unless (member symbol '(clostrum:run-time-environment
                                           clostrum:compilation-environment))
                     collect (symbol-name symbol))))
