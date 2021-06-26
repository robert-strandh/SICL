(cl:in-package #:sicl-boot-phase-3)

(defun define-ensure-class-using-class (e1 e2 e3)
  (let ((client (env:client e3)))
    (setf (env:fdefinition
           client e2 'sicl-clos:ensure-class-using-class)
          (lambda (class-or-nil
                   class-name
                   &rest keys
                   &key
                     name
                     direct-superclasses
                     (metaclass 'standard-class)
                   &allow-other-keys)
            (loop while (remf keys :metaclass))
            (cond ((typep class-or-nil 'class)
                   class-or-nil)
                  ((null class-or-nil)
                   (setf (env:find-class client e3 class-name)
                         (apply (env:fdefinition client e1 'make-instance)
                                metaclass
                                :name (make-symbol (symbol-name name))
                                :direct-superclasses
                                (loop for class-or-name in direct-superclasses
                                      collect (if (symbolp class-or-name)
                                                  (env:find-class client e3 class-or-name)
                                                  class-or-name))
                                keys)))
                  (t
                   (error 'type-error
                          :expected-type '(or null class)
                          :datum class-or-nil)))))))

(defun define-ensure-class (e2 e3)
  (let ((client (env:client e2)))
    (with-intercepted-function-cells
        (e3
         (sicl-clos:ensure-class-using-class
          (env:function-cell
           client e2 'sicl-clos:ensure-class-using-class)))
      (load-source-file "CLOS/ensure-class.lisp" e3))))


(defun enable-defclass (e1 e2 e3)
  (define-ensure-class-using-class e1 e2 e3)
  (define-ensure-class e2 e3))
