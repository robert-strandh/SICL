(cl:in-package :sicl-cons)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

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
          "both keyword arguments :test and :test-not were given."))

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
