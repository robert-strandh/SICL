(cl:in-package #:sicl-boot-phase-1)

(defun add-readers (environment function-names class slot-name slot-definition)
  (let ((function (compile nil `(lambda (args next-methods)
                                  (declare (ignore next-methods))
                                  (slot-value (car args) ',slot-name)))))
    (loop for function-name in function-names
          for gf = (sicl-genv:fdefinition function-name environment)
          for method = (make-instance 'closer-mop:standard-reader-method
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
    (loop for function-name in function-names
          for gf = (sicl-genv:fdefinition function-name environment)
          for method = (make-instance 'closer-mop:standard-writer-method
                         :lambda-list '(value object)
                         :qualifiers '()
                         :specializers (list (find-class t) class)
                         :function function
                         :slot-definition slot-definition)
          do (add-method gf method))))
