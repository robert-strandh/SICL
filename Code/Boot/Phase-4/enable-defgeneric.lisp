(cl:in-package #:sicl-boot-phase-3)

(defun define-classp (e4)
  (load-source-file "CLOS/classp-defgeneric.lisp" e4)
  (load-source-file "CLOS/classp-defmethods.lisp" e4))

(defun define-sub-specializer-p (e4)
  (load-source-file "CLOS/sub-specializer-p.lisp" e4))

(defun define-compute-applicable-methods (e4)
  (with-intercepted-function-cells
      (e4
       (class-of
        (list (lambda (object)
                (slot-value object 'sicl-boot::%class)))))
    (load-source-file "CLOS/compute-applicable-methods-support.lisp" e4))
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e4)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e4))

(defun define-compute-effective-method (e4)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e4)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e4)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e4))

(defun define-compute-discriminating-function (e4)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (class-of
        (list (lambda (object)
                (cond ((typep object 'sicl-boot::header)
                       (slot-value object 'sicl-boot::%class))
                      ((typep object 'fixnum)
                       (env:find-class (env:client e4) e4 'fixnum))
                      ((integerp object)
                       (env:find-class (env:client e4) e4 'integer))
                      ((null object)
                       (env:find-class (env:client e4) e4 'null))
                      ((symbolp object)
                       (env:find-class (env:client e4) e4 'symbol))
                      ((characterp object)
                       (env:find-class (env:client e4) e4 'character))
                      ((consp object)
                       (env:find-class (env:client e4) e4 'cons))
                      (t
                       (error "class of ~s asked for~%" object))))))
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function))
       (no-applicable-method
        (list #'no-applicable-method)))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e4))
  (load-source-file "CLOS/discriminating-automaton.lisp" e4)
  (define-error-functions '(sicl-clos::compute-test-tree) e4)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e4)
  (define-error-functions '(sicl-clos::make-cdr) e4)
  (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e4)
  (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e4))

(defun enable-compute-discriminating-function (e4)
  (define-classp e4)
  (define-sub-specializer-p e4)
  (define-compute-applicable-methods e4)
  (define-compute-effective-method e4)
  (define-compute-discriminating-function e4))

(defun enable-defgeneric (e3 e5)
  (let ((client (env:client e5)))
    (setf (env:fdefinition client e5 'ensure-generic-function)
          (lambda (function-name &rest arguments &key &allow-other-keys)
            (let ((args (copy-list arguments)))
              (loop while (remf args :environment))
              (loop while (remf args :generic-function-class))
              (if (env:fboundp client e5 function-name)
                  (env:fdefinition client e5 function-name)
                  (setf (env:fdefinition client e5 function-name)
                        (apply (env:fdefinition (env:client e3) e3 'make-instance)
                               (env:find-class (env:client e3) e3 'standard-generic-function)
                               :name function-name
                               :method-class (env:find-class (env:client e3) e3 'standard-method)
                               args)))))))
  (load-source-file "CLOS/defgeneric-defmacro.lisp" e5))

(defun define-generic-function-class-names (e5)
  (setf (env:fdefinition
         (env:client e5) e5 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore name environment))
          (values 'standard-generic-function 'standard-method))))
