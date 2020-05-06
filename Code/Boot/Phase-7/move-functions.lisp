(cl:in-package #:sicl-boot-phase-7)

(defun move-functions (e5 e6)
  (loop for name in '(sicl-clos::reader-method-class-default
                      sicl-clos::writer-method-class-default
                      sicl-clos::direct-slot-definition-class-default
                      sicl-clos::ensure-method
                      sicl-clos::shared-initialize-default-using-class)
        do (setf (sicl-genv:fdefinition name e5)
                 (sicl-genv:fdefinition name e6))))
