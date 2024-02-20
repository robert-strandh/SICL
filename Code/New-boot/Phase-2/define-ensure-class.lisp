(cl:in-package #:sicl-new-boot-phase-2)

;;; The arguments received by ENSURE-CLASS are not quite as
;;; documented, because we can make certain shortcuts here, given that
;;; this function is going to be called only from the expansion of
;;; DEFCLASS, and we have full control over that expansion.
(defun define-ensure-class (client e1 e2)
  (setf (clo:fdefinition client e2 'ensure-class)
        (lambda (name
                 &key
                   direct-default-initargs
                   direct-slots
                   direct-superclasses
                   metaclass)
          (let ((superclasses
                  (loop for class-name in direct-superclasses
                        collect (clo:find-class client e2 class-name t))))
            (setf (clo:find-class client e2 name)
                  (make-instance
                      (clo:find-class client e1 metaclass t)
                    :direct-default-initargs direct-default-initargs
                    :direct-slots direct-slots
                    :direct-superclasse superclasses
                    :name (make-symbol (symbol-name name))))))))
