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

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defmethod acclimation:report-condition ((c both-test-and-test-not-given)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           Both keyword arguments :test and :test-not were given."
          (name c)
          (name-package (name c))))

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

(defmethod acclimation:report-condition ((c must-be-proper-list)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           A proper list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-proper-or-circular-list)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           A proper or circular list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-proper-or-dotted-list)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           A proper or dotted list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-property-list)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           A property list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c must-be-association-list)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           A association list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c at-least-one-list-required)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           At least one list argument is required,~@
           but none was given."
          (name c)
          (name-package (name c))))
          
(defmethod acclimation:report-condition ((c at-least-one-argument-required)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           At least one argument is required,~@
           but none was given."
          (name c)
          (name-package (name c))))

(defmethod acclimation:report-condition ((c lists-must-have-the-same-length)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package):~@
           The two lists passed as arguments must~@
           have the same length, but the following~@
           was given:~@
           ~s~@
           and~@
           ~s."
          (name c)
          (name-package (name c))
          (list1 c)
          (list2 c)))

(defmethod acclimation:report-condition ((c setf-c*r-must-be-cons)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In the SETF expander for ~a (in the ~a package),~@
           the ~aargument ~s~@
           must be a cons cell, but the following was given instead:~@
           ~s."
          (name c)
          (name-package (name c))
          (if (zerop (length (access-string c)))
              ""
              (format nil "C~aR of the " (access-string c)))
          (original-tree c)
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c setf-nth-must-be-cons)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In the SETF expander for ~a (in the ~a package),~@
           the ~:R CDR of the argument ~s~@
           must be a CONS cell, but the following was given instead:~@
           ~s."
          (name c)
          (name-package (name c))
          (cons-cell-count c)
          (original-tree c)
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c warn-both-test-and-test-not-given)
                                          stream
                                          (language acclimation:english))
  (format stream
          "In ~a (in the ~a package),~@
           both keyword arguments :test and :test-not were given."
          (name c)
          (name-package (name c))))

(defmethod acclimation:report-condition
    ((c expected-list-with-at-least-n-elements)
     stream
     (language acclimation:english))
  (format stream
          "Expected a list with at least ~d elements,~@
           but the following was given instead:~@
           ~s."
          (at-least c)
          (found c)))
