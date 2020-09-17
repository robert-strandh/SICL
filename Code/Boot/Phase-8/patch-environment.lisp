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

(defun maybe-patch-generic-function-accessor-methods (generic-function e5)
  (let ((standard-reader-method-class
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e5))
        (standard-writer-method-class
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e5)))
    (loop for method in (funcall (sicl-genv:fdefinition 'sicl-clos:generic-function-methods e5) generic-function)
          for class = (slot-value method 'sicl-boot::%class)
          when (member class (list standard-reader-method-class standard-writer-method-class))
            do (maybe-patch-accessor-method method e5))))

(defun patch-accessor-methods (environment)
  (flet ((process-function (function)
           (when (and (typep function 'sicl-boot::header)
                      (eq (slot-value function 'sicl-boot::%class)
                          (sicl-genv:find-class 'standard-generic-function environment)))
             (maybe-patch-generic-function-accessor-methods function environment))))
    (let ((table (make-hash-table :test #'eq)))
      (do-all-symbols (symbol)
        (unless (gethash symbol table)
          (setf (gethash symbol table) t)
          (when (and (sicl-genv:fboundp symbol environment)
                     (not (sicl-genv:special-operator symbol environment))
                     (null (sicl-genv:macro-function symbol environment)))
            (process-function (sicl-genv:fdefinition symbol environment)))
          (when (sicl-genv:fboundp `(setf ,symbol) environment)
            (process-function (sicl-genv:fdefinition `(setf ,symbol) environment))))))))
