(cl:in-package :sicl-cons)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

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
