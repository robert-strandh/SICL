(cl:in-package #:sicl-new-boot-phase-1)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun load-predicament (client environment global-environment)
  (clo:fmakunbound client global-environment 'error)
  (clo:fmakunbound client global-environment 'warn)
  (sb:ensure-asdf-system client environment "sicl-conditions")
  (setf (clo:fdefinition client global-environment 'macroexpand)
        (lambda (form environment)
          (declare (ignore environment))
          (values form nil)))
  (clo:make-variable client global-environment '*error-output*
                     *error-output*)
  (clo:make-variable client global-environment '*query-io*
                     *query-io*)
  (sb:ensure-asdf-system
   client environment "predicament-base" :load-system-file t)
  (let* ((symbol @predicament-asdf:*string-designators*)
         (value (clo:symbol-value client global-environment symbol)))
    (eval `(defparameter ,symbol ',value)))
  (sb:ensure-asdf-system
   client environment "predicament-packages-intrinsic")
  (setf (clo:find-class client global-environment 'string)
        (find-class 'string))
  (sb:ensure-asdf-system client environment "predicament-common"))
