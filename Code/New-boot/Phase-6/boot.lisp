(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun wrap-environment (environment)
  (make-instance 'trucler-reference:environment
    :global-environment environment))

(defun boot (boot)
  (format *trace-output* "**************** Phase 6~%")
  (let* ((e4 (sb:e4 boot))
         (w4 (wrap-environment e4))
         (c4 (make-instance 'client))
         (env:*client* c4)
         (env:*environment* e4)
         (client c4))
    (reinitialize-instance c4 :environment e4)
    (sb:ensure-asdf-system c4 w4 "regalia-class-hierarchy")
    (sb:ensure-asdf-system c4 w4 "sicl-array-make-array-instance")
    (sb:ensure-asdf-system c4 w4 "regalia-common")
    ;; We don't want to deal with the real UPGRADED-COMPLEX-PART-TYPE
    ;; right now, but it is required by ctype triggered by a TYPECASE
    ;; form in Alexandria, so we define it here.
    (setf (clo:fdefinition c4 e4 'upgraded-complex-part-type)
          (lambda (type-specifier &optional environment)
            (declare (ignore environment))
            (ecase type-specifier
              (single-float 'single-float)
              (double-float 'double-float))))
    ;; We don't want to deal with MACROEXPAND right now, because it
    ;; involves calling Trucler which has not been loaded yet.
    ;; MACROEXPAND is used by Predicament to check whether the form
    ;; given to RESTART-CASE to check whether the (expansion of the)
    ;; signaling form is one of a set of signaling functions, but we
    ;; can safely assume that these forms are not macro forms.  So we
    ;; can define macroexpand to return the original form.
    (setf (clo:fdefinition c4 e4 'macroexpand)
          (lambda (form &optional environment)
            (declare (ignore environment))
            (values form nil)))))
