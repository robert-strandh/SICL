(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

;;; In environment E-DESTINATION, define the function NAME-DESTINATION
;;; to be the function named NAME-SOURCE in environment E-SOURCE.
(defun straddle (client e-destination name-destination e-source name-source)
  (setf (clo:fdefinition client e-destination name-destination)
        (clo:fdefinition client e-source name-source)))

(defun define-straddle-functions (client ea eb)
  (straddle client
            ea @clostrophilia:^ensure-method-combination
            eb @clostrophilia:ensure-method-combination)
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
