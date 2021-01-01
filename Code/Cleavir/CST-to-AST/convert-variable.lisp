(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-variable (client cst environment)
  (let* ((symbol (cst:raw cst))
         (info (trucler:describe-variable client environment symbol)))
    (loop while (null info)
          do (restart-case (error 'trucler:no-variable-description
                                  :name symbol
                                  :origin (cst:source cst))
               (continue ()
                 :report (lambda (stream)
                           (format stream "Consider the variable as special."))
                 (setf info 
                       (make-instance 'trucler:special-variable-description
                                      :name symbol)))
               ;; This is identical to CONTINUE, but more specifically named.
               (consider-special ()
                 :report (lambda (stream)
                           (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
               (substitute (new-symbol)
                 :report (lambda (stream)
                           (format stream "Substitute a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq info (trucler:describe-variable client environment new-symbol)))))
    (convert-cst client cst info environment)))
