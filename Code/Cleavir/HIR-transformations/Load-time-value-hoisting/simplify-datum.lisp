(cl:in-package #:cleavir-load-time-value-hoisting)

(defmethod simplify-datum ((datum cleavir-ir:datum) system)
  (values))

(defmethod simplify-datum ((load-time-value-input cleavir-ir:load-time-value-input) system)
  (let ((form (cleavir-ir:form load-time-value-input)))
    (flet ((change-to-constant-input (value)
             (change-class load-time-value-input 'cleavir-ir:constant-input
               :value value)
             ;; After changing the class, simplify again.
             (simplify-datum load-time-value-input system)))
      (cond
        ;; Check whether FORM is a quoted form.
        ((and (consp form)
              (eq (first form) 'quote)
              (null (cddr form)))
         (change-to-constant-input (second form)))
        ;; Check whether FORM is a self-evaluating object.
        ((and (not (consp form))
              (not (symbolp form)))
         (change-to-constant-input form))
        ;; Otherwise, invoke the next method.
        (t
         (call-next-method))))))
