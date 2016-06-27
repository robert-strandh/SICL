;;;; Copyright (c) 2008 - 2015
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

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

(cl:in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition malformed-body
    (program-error name-mixin acclimation:condition)
  ((%body :initarg :body :reader body)))
     
(define-condition malformed-cond-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-cond-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition malformed-case-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-case-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition otherwise-clause-not-last
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-keys
    (program-error name-mixin acclimation:condition)
  ((%keys :initarg :keys :reader keys)))
     
(define-condition malformed-typecase-clauses
    (program-error name-mixin acclimation:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-typecase-clause
    (program-error name-mixin acclimation:condition)
  ((%clause :initarg :clause :reader clause)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition ccase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition etypecase-type-error
    (type-error name-mixin acclimation:condition)
  ())

(define-condition ctypecase-type-error
    (type-error name-mixin acclimation:condition)
  ())
