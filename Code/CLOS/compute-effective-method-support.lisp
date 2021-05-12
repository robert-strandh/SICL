(cl:in-package #:sicl-clos)

(defun wrap-make-method-form
    (form arguments-var next-methods-var)
  `(lambda (,arguments-var ,next-methods-var)
     (flet ((next-method-p ()
              (not (null ,next-methods-var)))
            (call-next-method (&rest arguments)
              (funcall (method-function (first ,next-methods-var))
                       (if (null arguments)
                           ,arguments-var
                           arguments)
                       (rest ,next-methods-var))))
       ,form)))

(defun wrap-in-call-method-macrolet (form arguments-var)
  `(macrolet ((call-method (method &optional next-method-list)
                (cond ((and (consp method)
                            (eq (first method) 'make-method)
                            (null (rest (rest method))))
                       `(funcall ,(compile
                                   nil
                                   `(lambda ()
                                      ,(wrap-in-call-method-macrolet (second method) ',arguments-var)))))
                      ((not (consp method))
                       `(funcall ,(method-function method)
                                 ,',arguments-var
                                 (list ,@next-method-list)))
                      (t (error "Malformed argument to CALL-METHOD ~s" method)))))
     ,form))

(defun wrap-in-make-method-macrolet (form method-class)
  `(macrolet ((make-method (make-method-form)
                (let ((arguments-var (gensym))
                      (next-methods-var (gensym)))
                  (make-instance ,method-class
                    :qualifiers '()
                    :lambda-list '()
                    :specializers '()
                    :function
                    (compile nil
                     `(lambda (,arguments-var ,next-methods-var)
                        (declare (ignorable ,arguments-var))
                        (declare (ignore ,next-methods-var))
                        ,(wrap-in-call-method-macrolet make-method-form arguments-var)))))))
     ,form))

(defun wrap-method-combination-form (form method-class)
  (let ((arguments-var (gensym "ARGUMENTS-")))
    `(lambda (,arguments-var)
       ,(wrap-in-call-method-macrolet
         (wrap-in-make-method-macrolet form method-class)
         arguments-var))))

(defun compute-effective-method-default
    (generic-function method-combination methods)
  (let* ((method-qualifier-pairs
           (loop for method in methods
                 collect (cons method (method-qualifiers method))))
         (template (template method-combination))
         (variant-signature (variant-signature method-combination))
         (function (sicl-method-combination:effective-method-form-function template))
         (form (apply function method-qualifier-pairs variant-signature))
         (method-class (generic-function-method-class generic-function))
         (result (wrap-method-combination-form form method-class)))
    result))
