(cl:in-package #:sicl-boot-phase-2)

;;; We want to make it possible to load FASL files containing
;;; definitions of generic functions in E3 so that the result is the
;;; creation of a host standard generic function in E3.
;;;
;;; Defining a generic function involves calling
;;; ENSURE-GENERIC-FUNCTION, passing the name, the lambda list, and
;;; the environment as arguments.  For that reason,
;;; ENSURE-GENERIC-FUNCTION must exist in that environment, which in
;;; this case is E3.
;;;
;;; We can rely entirely on the host to execute the generic-function
;;; initialization protocol.

(defun define-ensure-generic-function-using-class (e1 e2 e3)
  (let ((client (env:client e3)))
    (setf (env:fdefinition
           client e2 'sicl-clos:ensure-generic-function-using-class)
          (lambda (generic-function-or-nil
                   function-name
                   &rest keys
                   &key
                     (generic-function-class 'standard-generic-function)
                   &allow-other-keys)
            (loop while (remf keys :environment))
            (loop while (remf keys :generic-function-class))
            (cond ((typep generic-function-or-nil 'generic-function)
                   generic-function-or-nil)
                  ((null generic-function-or-nil)
                   (setf (env:fdefinition client e3 function-name)
                         (apply (env:fdefinition client e1 'make-instance)
                                generic-function-class
                                :name function-name
                                keys)))
                  (t
                   (error 'type-error
                          :expected-type '(or null generic-function)
                          :datum generic-function-or-nil)))))))

(defun enable-defgeneric (e1 e2 e3)
  (define-ensure-generic-function-using-class e1 e2 e3)
  (let ((client (env:client e2)))
    (with-intercepted-function-cells
        (e3
         (sicl-clos:ensure-generic-function-using-class
          (env:function-cell
           client e2 'sicl-clos:ensure-generic-function-using-class)))
      (load-source-file "CLOS/ensure-generic-function-defun.lisp" e3))))
