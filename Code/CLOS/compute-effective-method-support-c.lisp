(cl:in-package #:sicl-clos)

(defun wrap-make-method-form
    (form generic-function-arguments-var method-arguments-var-p)
  (let* ((method-arguments-var (gensym "ARGUMENTS-"))
         (next-methods-var (gensym "NEXT-METHODS-"))
         (call-next-method-arguments-var (gensym "ARGUMENTS-"))
         (method-arguments-var-list
           (if method-arguments-var-p `(,method-arguments-var) '())))
   `(lambda (,@method-arguments-var-list ,next-methods-var)
       ,@(if method-arguments-var-p
             `((declare (ignore ,method-arguments-var)))
             '())
       (flet ((next-method-p ()
                (not (null ,next-methods-var)))
              (call-next-method (&rest ,call-next-method-arguments-var)
                (funcall (method-function (first ,next-methods-var))
                         (if (null ,call-next-method-arguments-var)
                             ,generic-function-arguments-var
                             ,call-next-method-arguments-var)
                         (rest ,next-methods-var))))
         ,form))))

(defun wrap-in-call-method-macrolet (form arguments-var)
  `(macrolet ((call-method (method &optional next-method-list)
                (cond ((and (consp method)
                            (eq (first method) 'make-method)
                            (null (rest (rest method))))
                       `(funcall ,(wrap-make-method-form
                                   (second method) ,arguments-var nil)
                                 (list ,next-method-list)))
                      ((not (consp method))
                       `(funcall (method-function ,method)
                                 ,',arguments-var
                                 (list ,next-method-list)))
                      (t (error "Malformed argument to CALL-METHOD ~s" method)))))
     ,form))

(defun wrap-in-make-method-macrolet-2 (form arguments-var method-class-name)
  `(macrolet ((make-method (make-method-form)
                `(make-instance ',',method-class-name
                   :qualifiers '()
                   :lambda-list '()
                   :specializers '()
                   :function ,(wrap-make-method-form
                               make-method-form ',arguments-var t))))
     ,form))
                                 
(defun wrap-in-make-method-macrolet (form arguments-var method-class-name)
  (let ((next-methods-var (gensym "NEXT-METHODS-"))
        (call-next-method-arguments-var (gensym "ARGUMENTS-")))
    `(macrolet
         ((make-method (make-method-form)
            `(make-instance ',',method-class-name
               :qualifiers '()
               :lambda-list '()
               :specializers '()
               :function
               (lambda (,',arguments-var ,',next-methods-var)
                 (flet ((next-method-p ()
                          (not (null ,',next-methods-var)))
                        (call-next-method (&rest ,',call-next-method-arguments-var)
                          (funcall (method-function (first ,',next-methods-var))
                                   (if (null ,',call-next-method-arguments-var)
                                       ,',arguments-var
                                       ,',call-next-method-arguments-var)
                                   (rest ,',next-methods-var))))
                   ,make-method-form)))))
       ,form)))
