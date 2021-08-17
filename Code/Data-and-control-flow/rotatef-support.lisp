(cl:in-package #:sicl-data-and-control-flow)

(defun rotatef-expander (places)
  (let* ((setf-expansions
           ;; Collect the SETF-EXPANSION of each place as a list of the
           ;; values returned by GET-SETF-EXPANSION. 
           (loop for place in places
                 collect (multiple-value-list
                          (get-setf-expansion place))))
         (result
           ;; We start by creating the body of the result, which
           ;; contains all the STORE-FORMs, storing the
           ;; STORE-VARIABLEs in the respective place.
           `(progn ,@(loop for setf-expansion in setf-expansions
                           for store-form = (fourth setf-expansion)
                           collect store-form)
                   ;; The HyperSpec says that ROTATEF returns NIL, so
                   ;; we make NIL the last element in the body of the
                   ;; result.
                   nil)))
    (loop for right-hand-side
            in (reverse (append (last setf-expansions)
                                (butlast setf-expansions)))
          for store-variables = (third right-hand-side)
          for (temporary-variables
               value-forms
               nil
               nil
               accessing-form)
            in (reverse setf-expansions)
          do (setf result
                   `(let* ,(loop for var in temporary-variables
                                 for form in value-forms
                                 collect `(,var ,form))
                      (multiple-value-bind ,store-variables ,accessing-form
                        ,result))))
    result))
