(cl:in-package #:sicl-boot)

(defun define-effective-slot-definition-class (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:effective-slot-definition-class (r2 boot))
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition
                                (r1 boot)))))
