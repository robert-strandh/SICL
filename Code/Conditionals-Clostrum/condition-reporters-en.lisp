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

(cl:in-package :sicl-conditionals)

(defmethod acclimation:report-condition ((condition malformed-body)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of forms,~@
           but the following was given instead:~@
           ~s"
          (body condition)))

(defmethod acclimation:report-condition ((condition malformed-cond-clauses)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of cond clauses,~@
           but the following was given instead:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-cond-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a cond clause of the form,~@
           (test-form form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))

(defmethod acclimation:report-condition ((condition malformed-case-clauses)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of case clauses,~@
           but the following was given instead:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-case-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a case clause of the form,~@
           (keys form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))

(defmethod acclimation:report-condition ((condition otherwise-clause-not-last)
                                          stream
                                          (language acclimation:english))
  (format stream
          "The `otherwise' or `t' clause must be last in a case form,~@
           but but it was followed by:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-keys)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a designator for a list of keys,~@
           but the following was given instead:~@
           ~s"
          (keys condition)))

(defmethod acclimation:report-condition ((condition malformed-typecase-clauses)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of typecase clauses,~@
           but the following was given instead:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-typecase-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a typecase clause of the form,~@
          (type form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(defmethod acclimation:report-condition ((condition ecase-type-error)
                                          stream
                                          (language acclimation:english))
  (format stream
          "No key matched in ecase expression.~@
           Offending datum:~@
           ~s~@
           Offending type:~@
           ~s"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(defmethod acclimation:report-condition ((condition ccase-type-error)
                                          stream
                                          (language acclimation:english))
  (format stream
          "No key matched in ccase expression.~@
           Offending datum:~@
           ~s~@
           Offending type:~@
           ~s"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(defmethod acclimation:report-condition ((condition etypecase-type-error)
                                          stream
                                          (language acclimation:english))
  (format stream
          "No key matched in etypecase expression.~@
           Offending datum:~@
           ~s~@
           Offending type:~@
           ~s"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(defmethod acclimation:report-condition ((condition ctypecase-type-error)
                                          stream
                                          (language acclimation:english))
  (format stream
          "No key matched in ctypecase expression.~@
           Offending datum:~@
           ~s~@
           Offending type:~@
           ~s"
          (type-error-datum condition)
          (type-error-expected-type condition)))
