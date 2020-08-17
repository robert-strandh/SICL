;;;; Copyright (c) 2013 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following
;;;; conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above
;;;;    copyright notice, this list of conditions and the following
;;;;    disclaimer in the documentation and/or other materials
;;;;    provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(cl:in-package #:common-lisp-user)

(defpackage #:sicl-global-environment
  (:nicknames #:sicl-env #:sicl-genv)
  (:use #:common-lisp)
  ;; When this package is defined in a host implementation for the
  ;; purpose of cross compilation, we shadow the symbols of the
  ;; COMMON-LISP package that name functions and variables that have
  ;; to do with manipulating the environment.
  (:shadow #:fboundp
           #:fdefinition
           #:macro-function
           #:compiler-macro-function
           #:boundp
           #:makunbound
           #:fmakunbound
           #:find-class
           #:find-package
           #:get-setf-expansion
           #:declaration
           #:symbol-plist
           #:class-of
           #:typep
           #:subtypep
           )
  (:export
   #:environment
   #:fboundp
   #:fdefinition
   #:macro-function
   #:compiler-macro-function
   #:function-type
   #:function-inline
   #:function-cell
   #:function-unbound
   #:function-lambda-list
   #:function-ast
   #:function-names
   #:constant-variable
   #:special-variable
   #:symbol-macro
   #:symbol-plist
   #:variable-type
   #:setf-expander
   #:default-setf-expander
   #:type-expander
   #:boundp
   #:makunbound
   #:fmakunbound
   #:variable-cell
   #:variable-unbound
   #:find-class
   #:find-package
   #:special-operator
   #:get-setf-expansion
   #:find-method-combination-template
   #:global-environment
   #:declaration
   #:declarations
   #:optimize-quality-values
   #:policy
   #:structure-description
   #:defun-expander
   #:class-of
   #:typep
   #:typep-compound
   #:subtypep
   #:client
   ))
