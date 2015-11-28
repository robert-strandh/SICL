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

(cl:in-package #:sicl-clos)

;;; FIXME: I can not remember why I decided not to use initargs for
;;; the slots here, and instead calling explicit writers in :AFTER
;;; methods on INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE.

(defclass generic-function (metaobject funcallable-standard-object)
  (;; While there is a function named (SETF GENERIC-FUNCTION-NAME), it
   ;; is not a writer function in that it works by calling
   ;; REINITIALIZE-INSTANCE.
   (%name 
    :initform nil
    :initarg :name
    :reader generic-function-name)
   (%lambda-list 
    :initarg :lambda-list
    :reader generic-function-lambda-list)
   (%documentation 
    :initarg :documentation
    :initform nil
    :accessor documentation)
   (%dependents
    :initform '()
    :accessor dependents))
  (:metaclass funcallable-standard-class))
