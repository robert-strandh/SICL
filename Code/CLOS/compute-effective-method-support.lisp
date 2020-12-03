(cl:in-package #:sicl-clos)

(defun wrap-make-method-form
    (form arguments-var arguments-var-p next-methods-var)
  (let* ((arguments-var-list
           (if arguments-var-p `(,arguments-var) '())))
    `(lambda (,@arguments-var-list ,next-methods-var)
       ,@(if arguments-var-p
             '()
             `((declare (ignore ,arguments-var))))
       (flet ((next-method-p ()
                (not (null ,next-methods-var)))
              (call-next-method (&rest ,arguments-var)
                (funcall (method-function (first ,next-methods-var))
                         ,arguments-var
                         (rest ,next-methods-var))))
         ,form))))

(defun wrap-in-call-method-macrolet (form arguments-var next-methods-var)
  `(macrolet ((call-method (method &optional next-method-list)
                (cond ((and (consp method)
                            (eq (first method) 'make-method)
                            (null (rest (rest method))))
                       `(funcall ,(wrap-make-method-form
                                   (second method)
                                   ',arguments-var
                                   nil
                                   ',next-methods-var)
                                 (list ,@next-method-list)))
                      ((not (consp method))
                       `(funcall ,(method-function method)
                                 ,',arguments-var
                                 (list ,@next-method-list)))
                      (t (error "Malformed argument to CALL-METHOD ~s" method)))))
     ,form))

(defun wrap-in-make-method-macrolet
    (form method-class arguments-var next-methods-var)
  `(macrolet ((make-method (make-method-form)
                (make-instance ,method-class
                  :qualifiers '()
                  :lambda-list '()
                  :specializers '()
                  :function
                  (lambda (,arguments-var ,next-methods-var)
                    ,(wrap-in-call-method-macrolet
                      (wrap-make-method-form 'make-method-form
                                             arguments-var
                                             t
                                             next-methods-var)
                      arguments-var
                      next-methods-var)))))
     ,form))

(defun wrap-method-combination-form (form method-class)
  (let ((arguments-var (gensym "ARGUMENTS-"))
        (next-methods-var (gensym "NEXT-METHODS-")))
    `(lambda (,arguments-var)
       ,(wrap-in-call-method-macrolet
         (wrap-in-make-method-macrolet
          form method-class arguments-var next-methods-var)
         arguments-var next-methods-var))))

(defun compute-effective-method-default
    (generic-function method-combination methods)
  (let* ((method-qualifier-pairs
           (loop for method in methods
                 collect (cons method (method-qualifiers method))))
         (template (template method-combination))
         (variant-signature (variant-signature method-combination))
         (function (sicl-method-combination:effective-method-form-function template))
         (form (apply function method-qualifier-pairs variant-signature))
         (method-class (generic-function-method-class generic-function)))
    (wrap-method-combination-form form method-class)))
