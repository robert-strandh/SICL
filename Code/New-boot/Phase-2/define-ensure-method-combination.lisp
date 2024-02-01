(cl:in-package #:sicl-new-boot-phase-2)

(defun define-^ensure-method-combination (client e1 e2)
  (let ((symbol1
          (sb:intern-parcl-symbol
           client "CLOSTROPHILIA" "ENSURE-METHOD-COMBINATION"))
        (symbol2
          (sb:intern-parcl-symbol
           client "CLOSTROPHILIA" "^ENSURE-METHOD-COMBINATION")))
    (setf (clo:fdefinition client e2 symbol2)
          (clo:fdefinition client e1 symbol1))))
