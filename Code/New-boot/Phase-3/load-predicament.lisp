(cl:in-package #:sicl-new-boot-phase-3)

(defun load-predicament (client environment global-environment)
  ;; Predicament defines ERROR and WARN as generic functions, but
  ;; currently, ERROR and WARN are imported from the host, so we
  ;; need to remove them first.
  (clo:fmakunbound client global-environment 'error)
  (clo:fmakunbound client global-environment 'warn)
  (sb:ensure-asdf-system client environment "sicl-conditions")
  ;; The macro RESTART-CASE analyzes the REPORT expression using a
  ;; TYPECASE form which expands to a sequence of calls to TYPEP, so
  ;; we need to define TYPEP to handle those cases before we can
  ;; load Predicament.  We must not forget to replace TYPEP later,
  ;; because it is not fit to be used to determine the type of
  ;; objects in E3.
  (setf (clo:fdefinition client global-environment 'typep)
        (lambda (object type-specifier)
          (ecase type-specifier
            (null (null object))
            (string (stringp object)))))
  ;; Predicament calls MACROEXPAND as part of the RESTART-CASE macro,
  ;; in order to determine whether the signaling form is one of
  ;; SIGNAL, WARN, ERROR, or CERROR.  Since the signaling form could
  ;; be a macro form that expands to one of these, Predicament calls
  ;; MACROEXPAND.  This behavior is documented in the Common Lisp
  ;; standard.
  (setf (clo:fdefinition client global-environment 'macroexpand)
        (lambda (form environment)
          (declare (ignore environment))
          (values form nil)))
  (clo:make-variable client global-environment '*error-output*
                     *error-output*)
  (clo:make-variable client global-environment '*query-io*
                     *query-io*)
  (sb:ensure-asdf-system client environment "predicament-common")
  (clo:fmakunbound client global-environment 'typep)
  (clo:fmakunbound client global-environment 'macroexpand))
