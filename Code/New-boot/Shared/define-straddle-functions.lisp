(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

;;; In environment E-DESTINATION, define the function NAME-DESTINATION
;;; to be the function named NAME-SOURCE in environment E-SOURCE.
(defun straddle (client e-destination name-destination e-source name-source)
  (setf (clo:fdefinition client e-destination name-destination)
        (clo:fdefinition client e-source name-source)))

(defun define-straddle-functions (client ea eb)
  (straddle client
            eb @clostrophilia:slot-value-using-location+1
            ea @clostrophilia:slot-value-using-location)
  (straddle client
            eb (list 'setf @clostrophilia:slot-value-using-location+1)
            ea (list 'setf @clostrophilia:slot-value-using-location))
  (straddle client
            ea @clostrophilia:slot-boundp-using-only-class-1
            eb @clostrophilia:slot-boundp-using-only-class)
  (straddle client
            ea @clostrophilia:slot-value-using-only-class-1
            eb @clostrophilia:slot-value-using-only-class)
  (straddle client
            ea (list 'setf @clostrophilia:slot-value-using-only-class-1)
            eb (list 'setf @clostrophilia:slot-value-using-only-class))
  (straddle client
            ea @clostrophilia:slot-makunbound-using-only-class-1
            eb @clostrophilia:slot-makunbound-using-only-class)
  (straddle client
            ea @clostrophilia:^ensure-method-combination
            eb @clostrophilia:ensure-method-combination)
  (straddle client
            ea @sicl-clos:^ensure-generic-function-using-class
            eb @sicl-clos:ensure-generic-function-using-class)
  (straddle client
            ea @sicl-clos:^ensure-class-using-class
            eb @sicl-clos:ensure-class-using-class)
  (straddle client
            ea @sicl-clos:^ensure-method-using-generic-function
            eb @sicl-clos:ensure-method-using-generic-function)
  (straddle client
            ea @sicl-clos:^ensure-method-combination-template-using-class
            eb @sicl-clos:ensure-method-combination-template-using-class)
  (straddle client
            ea @sicl-clos:^method-function
            eb @clostrophilia:method-function))
