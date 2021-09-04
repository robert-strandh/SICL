(cl:in-package #:sicl-boot-phase-6)

(defun load-sequence-functions (e5)
  ;; E5 already contains sequence functions imported from the host.
  ;; But those imported functions are not SICL generic functions.  So
  ;; when we attempt to load a DEFGENERIC form for a function with the
  ;; name of one of those functions, ENSURE-GENERIC-FUNCTION is going
  ;; to return that imported function, which obviously will not work.
  ;; We could start by calling FMAKUNBOUND for all those imported
  ;; function, but that does not work either, because they are needed
  ;; in order to load things.  So the way we do that is that we wrap
  ;; ENSURE-GENERIC-FUNCTION so that it removes the existing function
  ;; if it is a host function (ordinary or generic.
  (let* ((old (env:fdefinition (env:client e5) e5 'ensure-generic-function))
         (new (lambda (name &rest arguments)
                (when (env:fboundp (env:client e5) e5 name)
                  (let ((class (class-of (env:fdefinition (env:client e5) e5 name))))
                    (unless (eq class (find-class 'sicl-boot:header))
                      (env:fmakunbound (env:client e5) e5 name))))
                (apply old name arguments))))
    (setf (env:fdefinition (env:client e5) e5 'ensure-generic-function)
          new))
  (load-asdf-system '#:fast-generic-functions e5)
  (setf (env:proclamation (env:client e5) e5 'declaration)
        (list (find-symbol (symbol-name '#:method-properties)
                           '#:fast-generic-functions))))
