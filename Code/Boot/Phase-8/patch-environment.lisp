(cl:in-package #:sicl-boot-phase-8)

(defun maybe-patch-accessor-method (accessor-method e5)
  (flet ((fun (name)
           (sicl-genv:fdefinition name e5)))
    (unless (typep (funcall (fun 'sicl-clos:method-function) accessor-method)
                   'sicl-boot::header)
      (let* ((slot-definition
               (funcall (fun 'sicl-clos:accessor-method-slot-definition)
                        accessor-method))
             (slot-name
               (funcall (fun 'sicl-clos:slot-definition-name) slot-definition)))
        (funcall (fun 'reinitialize-instance)
                 accessor-method
                 :function
                 (funcall (fun 'compile)
                          nil
                          `(lambda (arguments next-methods)
                             (declare (ignore next-methods))
                             ,(if (eq (funcall (fun 'class-of) accessor-method)
                                      (sicl-genv:find-class 'sicl-clos:standard-reader-method e5))
                                  `(slot-value (car arguments) ',slot-name)
                                  `(setf (slot-value (cadr arguments) ',slot-name)
                                         (car arguments))))))
        (setf (slot-value slot-definition 'sicl-boot::%class)
              (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e5))))))

        
  
