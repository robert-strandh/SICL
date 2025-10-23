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
            (values form nil)))
    ;; [*sigh*] RESTART-CASE uses TYPECASE with STRING as one of the
    ;; cases, but (TYPEP "mumble" 'STRING) returns false, so the
    ;; expansion of the TYPECASE expression is wrong, resulting in a
    ;; compilation error.  The only way I can think of fixing this is
    ;; to define TYPEP to check for host strings first.
    (let ((old (clo:fdefinition c4 e4 'typep)))
      (setf (clo:fdefinition c4 e4 'typep)
            (lambda (object type-specifier &optional (environment e4))
              (or (and (eq type-specifier 'string) (stringp object))
                  (funcall old object type-specifier environment)))))
    (setf (clo:macro-function c4 e4 'with-standard-io-syntax)
          (lambda (form environment)
            (declare (ignore environment))
            `(progn ,@(rest form))))
    (sb:ensure-asdf-system c4 w4 "sicl-data-and-control-flow-setf-expanders")
    (clo:make-variable client e4 '*features* '(:sicl))
    (let ((*features* *features*))
      (setf *features* (remove :sbcl *features*))
      (setf *features* (remove :sb-package-locks *features*))
      (sb:ensure-asdf-system c4 w4 "alexandria"))
    (sb:ensure-asdf-system c4 w4 "sicl-posix-low-package")
    (sb:ensure-asdf-system c4 w4 "sicl-posix-high-package")
    (setf sicl-new-boot::*c4* c4)
    (setf sicl-new-boot::*w4* w4)
    (setf sicl-new-boot::*e4* e4)
    (let ((*features* '(:sicl)))
      (sb:ensure-asdf-system c4 w4 "trivial-package-locks")
      (sb:ensure-asdf-system c4 w4 "trinsic")
      (sb:ensure-asdf-system c4 w4 "cyclosis")
      (sb:ensure-asdf-system c4 w4 "cyclosis-intrinsic"))
    (clo:make-variable
     client e4 @predicament:*condition-maker* 'make-condition)
    (load-quaviver c4 w4 e4)))
