(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun wrap-environment (environment)
  (make-instance 'trucler-reference:environment
    :global-environment environment))

(defun boot (boot)
  (format *trace-output* "**************** Phase 5~%")
  (let* ((e3 (sb:e3 boot))
         (e4 (sb:e4 boot))
         (c3 (make-instance 'client))
         (c4 (make-instance 'client))
         (env:*client* c4)
         (env:*environment* e4)
         (client c4))
    (reinitialize-instance c3 :environment e3)
    (reinitialize-instance c4 :environment e4)
    (sb:ensure-asdf-system
     c3 (wrap-environment e3) "sicl-clos-satiation")
    (satiate-metaobject-functions-1 c3 e3 e4)
    ;; In phase 4, we set FIND-CLASS+1 in E3 to refer to FIND-CLASS in
    ;; E4, because MAKE-INSTANCE calls FIND-CLASS+1.  In the following
    ;; code, we detect functions that are shared between E3 and E4,
    ;; and we assume that the function in E4 must be changed, but
    ;; that's not the case for FIND-CLASS.  So we remove FIND-CLASS+1
    ;; from E3, and we set FIND-CLASS+1 in E4, to refer to FIND-CLASS
    ;; in E4.
    (flet ((fixup (name+1 name)
             (clo:fmakunbound c3 e3 name+1)
             (setf (clo:fdefinition c4 e4 name+1)
                   (clo:fdefinition c4 e4 name))))
      (fixup @sicl-clos:find-class+1 @sicl-clos:find-class)
      (fixup @clostrophilia:slot-boundp-using-location+1
             @clostrophilia:slot-boundp-using-location)
      (let ((s+1 @clostrophilia:slot-value-using-location+1)
            (s @clostrophilia:slot-value-using-location))
        (fixup s+1 s)
        (fixup `(setf ,s+1) `(setf ,s)))
      (fixup @clostrophilia:slot-makunbound-using-location+1
             @clostrophilia:slot-makunbound-using-location)
      (fixup @clostrophilia:ensure-generic-function+1
             'ensure-generic-function))))
