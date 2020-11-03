(cl:in-package #:sicl-boot-phase-2)

(defun prepare-next-phase (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (import-functions-from-host
     '(reinitialize-instance)
     e3)
    (enable-defgeneric e3)
    (enable-defmethod e2 e3)
    (enable-class-initialization boot)
    (enable-defclass boot)))
