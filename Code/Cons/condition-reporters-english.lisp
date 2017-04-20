(cl:in-package :sicl-cons)

;;;; Copyright (c) 2008 - 2016
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

(defmethod acclimation:report-condition ((c must-be-nonnegative-integer)
					  stream
					  (language acclimation:english))
  (format stream
	  "A nonnegative integer was required,~@
           but the following was given:~@
           ~s"
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-cons)
					  stream
					  (language acclimation:english))
  (format stream
	  "A cons cell was required,~@
           but the following was given:~@
           ~s"
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-list)
					  stream
					  (language acclimation:english))
  (format stream
	  "A list (a cons or nil) was required,~@
           but the following was given:~@
           ~s"
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-plist)
                      stream
                      (language acclimation:english))
  (format stream
      "A property list was required, ~@
           but the following was given:~@
           ~s"
      (type-error-datum c)))
