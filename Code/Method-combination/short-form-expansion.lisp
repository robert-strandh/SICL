(cl:in-package #:sicl-method-combination)

(defun short-form-expander (name options)
  (unless (cleavir-code-utilities:proper-list-p options)
    (error "options must be a proper list"))
  (unless (evenp (length options))
    (error "options must be a list with an even number of elements"))
  (let ((table (make-hash-table :test #'eq)))
    (loop for (option value) on options by #'cddr
          do (unless (null (nth-value 1 (gethash option table)))
               (error "option ~s given more than once" option))
             (unless (member option
                             '(:documentation
                               :identity-with-one-argument
                               :operator))
               (error "unknown option ~s" option))
             (setf (gethash option table) value))
    (multiple-value-bind (value present-p) (gethash :operator table)
      (let ((operator (if present-p value name)))
        `(define-method-combination ,name
             (&optional (order :most-specific-first))
           ((around (:around))
            (primary (,name) :order order :required t))
           (let ((form (if (null (rest primary))
                           ,(if (gethash :identity-with-one-argument table)
                                ``(call-method ,(first primary))
                                ``(,',operator (call-method ,(first primary))))
                           `(,',operator ,@(loop for method in primary
                                                 collect `(call-method ,method))))))
             (if (null around)
                 form
                 `(call-method ,(first around)
                               (,@(rest around)
                                (make-method ,form))))))))))
