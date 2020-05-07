(cl:in-package #:sicl-boot-phase-7)

(defun satiate-generic-functions (e3 e4 e5)
  (flet ((satiate (function)
           (funcall (sicl-genv:fdefinition 'sicl-clos::satiate-generic-function e4)
                    function)))
    (let ((name-fun (sicl-genv:fdefinition 'sicl-clos:generic-function-name e4)))
      (do-all-symbols (var)
        (when (sicl-genv:fboundp var e5)
          (let ((fun (sicl-genv:fdefinition var e5)))
            (when (and (typep fun 'sicl-boot::header)
                       (eq (slot-value fun 'sicl-boot::%class)
                           (sicl-genv:find-class 'standard-generic-function e3)))
              (format *trace-output*
                      "Satiating ~s~%"
                      (funcall name-fun fun))
              (satiate fun))))))))

(defun boot (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (satiate-generic-functions e3 e4 e5)
    (patch-classes e4 e5)
    (patch-generic-functions e3 e4 e5)
    (move-functions e5 e6)))
