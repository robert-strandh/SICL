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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REAL-CLASS.

(defclass real-class (class)
  ((%direct-default-initargs
    :initarg :direct-default-initargs
    :initform '()
    :reader class-direct-default-initargs)
   (%documentation
    :initform nil
    :accessor documentation)
   (%precedence-list
    :initform '()
    ;; The AMOP says that CLASS-PRECEDENCE-LIST should signal an error
    ;; if the class has not been finalized.  We accomplish that effect
    ;; by defining a :BEFORE method that checks this condition, and
    ;; signals an error of the class has not been finalized.
    :reader class-precedence-list
    ;; During class finalization, we need to set the value of this
    ;; slot, so we need a writer for it, and that writer should not be
    ;; named CLASS-PRECEDENCE-LIST because that name is exported.
    ;; Furthermore, also during class finalization, once the class
    ;; precedence list has been computed and store, and we need to
    ;; compute the effective slots and the default initargs, these
    ;; last two steps need to access the precedence list.  However,
    ;; because the function CLASS-PRECEDENCE-LIST signals an error if
    ;; the class is not finalized, those last two steps can not use
    ;; it.  We therefore also need an alternative reader for this slot
    ;; (we could have used SLOT-VALUE, but we prefer a reader which is
    ;; typically faster).  Our solution is to define the ACCESSOR
    ;; named PRECEDENCE-LIST.
    :accessor precedence-list)
   ;; ALLOCATE-INSTANCE and ALLOCATE-BUILT-IN-INSTANCE access this
   ;; slot in order to determine the size of the instance to allocate.
   ;; The writer is used during class finalization.
   (%instance-size :accessor instance-size)))
