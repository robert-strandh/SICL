;;;; Copyright (c) 2010 - 2015
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

(cl:in-package #:sicl-clos)

(defclass standard-generic-function (generic-function)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader generic-function-argument-precedence-order)
   (%declarations 
    :initarg :declarations
    :reader generic-function-declarations)
   (%method-class 
    :initarg :method-class
    :reader generic-function-method-class)
   (%method-combination 
    :initarg :method-combination
    ;; FIXME: remove this later.
    :initform nil
    :reader generic-function-method-combination)
   ;; This slot contains all the methods of the generic function,
   ;; including those that were defined as part of the DEFGENERIC
   ;; form.
   (%methods 
    :initform '() 
    :accessor generic-function-methods)
   ;; This slot contains only the methods that were defined as part of
   ;; the DEFGENERIC form.  We need it, because the Common Lisp
   ;; HyperSpec says that when a DEFGENERIC form is evaluated and the
   ;; generic function exists already, then the methods that were
   ;; added as a result of the evaluation of the DEFGENERIC form are
   ;; first removed.  That is not the case for methods defined by
   ;; separate DEFMETHOD forms.
   (%initial-methods
    :initform '()
    :accessor initial-methods)
   ;; We maintain a CALL HISTORY of the generic function.  This call
   ;; history is a list of call records.  Whenever a call is made to
   ;; the generic function with some call profile that has not yet
   ;; been used in a call, we compute the effective method to use, and
   ;; we add a call record to the call history.
   (%call-history 
    :initform '() 
    :accessor call-history)
   ;; This slot is set by ADD-METHOD and REMOVE-METHOD.
   ;; It cnotains a list that has the same length as the 
   ;; number of required parameters of the generic function.
   ;; It contains NIL in each position where only the class T
   ;; is specialized for, and T in each position where some
   ;; method specialized for something other than the class T.
   (%specializer-profile
    :initarg :specializer-profile
    :accessor specializer-profile))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :declarations '()
   :method-class (find-class 'standard-method)))

;;  LocalWords:  DEFGENERIC defgeneric
