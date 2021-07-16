(cl:in-package :sicl-cons)

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
