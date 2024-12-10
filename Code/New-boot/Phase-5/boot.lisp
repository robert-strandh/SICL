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
    (sb:ensure-asdf-system
     c4 (wrap-environment e4) "sicl-clos-satiation")
    (satiate-metaobject-functions c3 e3 e4)
    (fix-forward-referring-functions client e3 e4)
    (fix-backward-referring-functions client e3 e4)
    (fix-variables client e3 e4)
    ;; Normally, when a method function of a method in E4 calls some
    ;; other function, the callee will also be in E4.  However, when
    ;; the effective method involving a slot accessor is computer, the
    ;; code for computing the effective method substitutes a different
    ;; method function that calls SLOT-VALUE-USING-LOCATION+1, and
    ;; since the code for computing the effective method is so far in
    ;; E3, such a slot accessor will call SLOT-VALUE-USING-LOCATION+1
    ;; in E3.  But we made SLOT-VALUE-USING-LOCATION+1 in E3 unbound
    ;; when we fixed forward-referring functions, and we did that so
    ;; that it would not be incorrectly handled by the code for fixing
    ;; backward-referring functions.  Unfortunately, that means that,
    ;; at this point, slot accessors in E4 are not operational.  So we
    ;; must restore SLOT-VALUE-USING-LOCATION+1 to be an alias for
    ;; SLOT-VALUE-USING-LOCATION in E4 until we run the satiation code
    ;; in E4.
    (let ((s+1 @clostrophilia:slot-value-using-location+1)
          (s @clostrophilia:slot-value-using-location))
      (setf (clo:fdefinition c3 e3 s+1)
            (clo:fdefinition c4 e4 s))
      (setf (clo:fdefinition c3 e3 `(setf ,s+1))
            (clo:fdefinition c4 e4 `(setf ,s))))
    (setf (clo:fdefinition c4 e4 @sicl-clos:initialize-instance+1)
          (clo:fdefinition c4 e4 'initialize-instance))
    (tie-the-knot c4 e3 e4)
    (clo:make-variable c4 e4 @clostrophilia:*class-t+1*
                       (clo:find-class c4 e4 't))
    (setf (clo:fdefinition client e4 @clostrophilia:make-method-instance)
          (clo:fdefinition client e4 'make-instance))
    (setf (clo:fdefinition client e4 @clostrophilia:stamp+1)
          (clo:fdefinition client e4 @clostrophilia:stamp))
    (setf (clo:fdefinition client e4 @clostrophilia:class-of+1)
          (clo:fdefinition client e4 'class-of))
    (satiate-metaobject-functions c4 e4 e4)
    (set-default-discriminating-functions c4 e4)
    (setf (clo:fdefinition client e4 @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class client e4 't)))))
