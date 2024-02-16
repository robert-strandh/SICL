(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun define-^ensure-method-combination (client e1 e2)
  (setf (clo:fdefinition
         client e2 @clostrophilia:^ensure-method-combination)
        (clo:fdefinition
         client e1 @clostrophilia:ensure-method-combination)))
