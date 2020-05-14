(cl:in-package #:sicl-boot-phase-7)

(defun update-instance-slot-list (instance environment)
  (let* ((class (slot-value instance 'sicl-boot::%class))
         (fun (sicl-genv:fdefinition 'sicl-clos:class-slots environment))
         (slots (funcall fun class)))
    (setf (aref (slot-value class 'sicl-boot::%rack) 1)
          slots)))
