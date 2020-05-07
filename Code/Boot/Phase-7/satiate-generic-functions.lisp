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
