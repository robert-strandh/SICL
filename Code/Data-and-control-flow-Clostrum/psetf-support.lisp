(cl:in-package #:sicl-data-and-control-flow)

(defun psetf-expander (client environment pairs)
  (let* ((setf-expansions
           ;; Collect the SETF-EXPANSION of each place as a list of the
           ;; values returned by GET-SETF-EXPANSION. 
           (loop for place in pairs by #'cddr
                 collect (multiple-value-list
                          (sicl-environment:get-setf-expansion
                           client environment place))))
         (result
           ;; We start by creating the body of the result, which
           ;; contains all the STORE-FORMs, storing the
           ;; STORE-VARIABLEs in the respecctive place.
           (append (loop for setf-expansion in setf-expansions
                         for store-form = (fourth setf-expansion)
                         collect store-form)
                   ;; The HyperSpec says that PSETF returns NIL, so we
                   ;; make NIL the last element in the body of the
                   ;; result.
                   (list nil))))
    ;; We now traverse the FORMs and the SETF-EXPANSIONs in reverse,
    ;; and for each iteration, we wrap the current result in a LET* to
    ;; evaluate the sub forms of the place and in a
    ;; MULTIPLE-VALUE-BIND to bind the STORE-VARIABLEs to the values
    ;; of the respective FORM.
    (loop for form in (reverse pairs) by #'cddr
          for (temporary-variables
               value-forms
               store-variables)
            in (reverse setf-expansions)
          do (setf result
                   `(let* ,(loop for var in temporary-variables
                                 for form in value-forms
                                 collect `(,var ,form))
                      (multiple-value-bind ,store-variables ,form
                        ,result))))
    result))
