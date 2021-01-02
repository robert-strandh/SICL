(cl:in-package #:sicl-data-and-control-flow)

(define-setf-expander values (&environment environment &rest places)
  (let ((result-temporaries '())
        (result-value-forms '())
        (result-store-variables '())
        (result-storing-form '(values))
        (result-accessing-form '(values)))
    (loop for place in places
          do (multiple-value-bind
                   (temporaries value-forms store-variables storing-form accessing-form)
                 (get-setf-expansion place environment)
               (setf result-temporaries
                     (append result-temporaries temporaries
                             ;; We add as temporaries store variables
                             ;; beyond the first one.  The standard
                             ;; says that those store variables have
                             ;; to be given the value NIL.
                             (rest store-variables)))
               (setf result-value-forms
                     (append result-value-forms value-forms
                             ;; To assign NIL to those additional
                             ;; store variables, we add the same
                             ;; number of NILs to the end of the value
                             ;; forms.
                             (make-list (length (rest store-variables))
                                        :initial-element nil)))
               (setf result-store-variables
                     (append result-store-variables
                             (if (null store-variables)
                                 '()
                                 (list (first store-variables)))))
               (setf result-storing-form
                     (append result-storing-form (list storing-form)))
               (setf result-accessing-form
                     (append result-accessing-form (list accessing-form)))))
    (values result-temporaries
            result-value-forms
            result-store-variables
            result-storing-form
            result-accessing-form)))
