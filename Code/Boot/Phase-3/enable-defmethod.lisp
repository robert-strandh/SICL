(cl:in-package #:sicl-boot-phase-3)

(defun define-make-specializer-in-e2 (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e2)
        (lambda (specializer)
          (cond ((eq specializer 't)
                 (find-class 't))
                ((symbolp specializer)
                 (sicl-genv:find-class specializer e2))
                (t
                 (error "Specializer must be a symbol: ~s"
                        specializer))))))

(defun enable-defmethod (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (setf (sicl-genv:fdefinition 'ensure-generic-function e2)
          (sicl-genv:fdefinition 'ensure-generic-function e3))
    (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype e2)
          #'closer-mop:class-prototype)
    (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class e2)
          #'closer-mop:generic-function-method-class)
    (flet ((ld (name environment)
             (load-source name environment)))
      (ld "CLOS/make-method-for-generic-function.lisp" e2)
      (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e3)
            (sicl-genv:fdefinition 'sicl-clos::method-function e3))
      (define-make-specializer-in-e2 e2)
      (sicl-boot:with-straddled-function-definitions
          ((sicl-clos::ensure-method) e3)
        (ld "CLOS/ensure-method.lisp" e2)))))
