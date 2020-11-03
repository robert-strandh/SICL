(cl:in-package #:sicl-boot-phase-5)

(defun update-class-slot (object translation-table)
  (let* ((current-slot-value (slot-value object 'sicl-boot::%class))
         (translation (gethash current-slot-value translation-table)))
    (if (null translation)
        (format *trace-output*
                "Class slot ~s of object ~s is not in table~%"
                object current-slot-value)
        (setf (slot-value object 'sicl-boot::%class) translation))))
