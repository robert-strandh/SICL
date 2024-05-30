(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

(defun define-straddle-functions (client ea eb)
  (setf (clo:fdefinition
         client ea
         @clostrophilia:^ensure-method-combination)
        (clo:fdefinition
         client eb @clostrophilia:ensure-method-combination))
  (setf (clo:fdefinition
         client ea
         @sicl-clos:^ensure-generic-function-using-class)
        (clo:fdefinition
         client eb
         @sicl-clos:ensure-generic-function-using-class))
  (setf (clo:fdefinition
         client ea @sicl-clos:^ensure-class-using-class)
        (clo:fdefinition
         client eb @sicl-clos:ensure-class-using-class))
  (setf (clo:fdefinition
         client ea
         @sicl-clos:^ensure-method-using-generic-function)
        (clo:fdefinition
         client eb
         @sicl-clos:ensure-method-using-generic-function))
  (setf (clo:fdefinition
         client ea
         @sicl-clos:^ensure-method-combination-template-using-class)
        (clo:fdefinition
         client eb
         @sicl-clos:ensure-method-combination-template-using-class))
  (setf (clo:fdefinition
         client ea @sicl-clos:^method-function)
        (clo:fdefinition
         client eb @clostrophilia:method-function)))
