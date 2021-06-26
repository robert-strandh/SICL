(cl:in-package #:sicl-boot-phase-3)

(defun add-readers (environment function-names class slot-name slot-definition)
  (let ((function (compile nil `(lambda (args next-methods)
                                  (declare (ignore next-methods))
                                  (slot-value (car args) ',slot-name)))))
    (loop with client = (env:client environment)
          for function-name in function-names
          for gf = (env:fdefinition client environment function-name)
          for method = (make-instance 'sicl-host-mop:standard-reader-method
                         :lambda-list '(object)
                         :qualifiers '()
                         :specializers (list class)
                         :function function
                         :slot-definition slot-definition)
          do (add-method gf method))))

(defun add-writers (environment function-names class slot-name slot-definition)
  (let ((function (compile nil `(lambda (args next-methods)
                                  (declare (ignore next-methods))
                                  (setf (slot-value (cadr args) ',slot-name)
                                        (car args))))))
    (loop with client = (env:client environment)
          for function-name in function-names
          for gf = (env:fdefinition client environment function-name)
          for method = (make-instance 'sicl-host-mop:standard-writer-method
                         :lambda-list '(value object)
                         :qualifiers '()
                         :specializers (list (find-class t) class)
                         :function function
                         :slot-definition slot-definition)
          do (add-method gf method))))
