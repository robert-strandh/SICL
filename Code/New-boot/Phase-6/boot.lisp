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
    ;; When ctype was loaded in Phase 4, we intercepted references to
    ;; MAKE-INSTANCE because ctype calls make-instance as part of
    ;; being loaded, but that was make-instance in E3, and now we need
    ;; for ctype to call MAKE-INSTANCE in E4, and the simplest way to
    ;; do that is to set MAKE-INSTANCE in E3 to be make-instance of
    ;; E4.  But that's a temporary solution, because ctype now has a
    ;; cell that belong to E3.
    (setf (clo:fdefinition client (sb:e3 boot) 'make-instance)
          (clo:fdefinition client e4 'make-instance))
    ;; We don't want to deal with the real UPGRADED-COMPLEX-PART-TYPE
    ;; right now, but it is required by ctype triggered by a TYPECASE
    ;; form in Alexandria, so we define it here.
    (setf (clo:fdefinition c4 e4 'upgraded-complex-part-type)
          (lambda (type-specifier &optional environment)
            (declare (ignore environment))
            (ecase type-specifier
              (single-float 'single-float)
              (double-float 'double-float))))))
