(cl:in-package #:sicl-environment)

;;; This function differs from the standard Common Lisp function with
;;; the same name in that it has three required parameters.
;;;
;;; It is defined this way so that it can be executed by the host
;;; Common Lisp system early on during bootstrapping.
;;;
;;; It might seem that we could omit the CLIENT parameter, since it
;;; can be obtained from the global environment.  However, to get to
;;; the global environment, we can not call
;;; SICL-ENVIRONMENT:GLOBAL-ENVIRONMENT, because that function is not
;;; defined outside the Clostrum environment.  And the reason for
;;; that, is that it must be a closure, closing over the environment
;;; in which it is defined.
;;;
;;; Now, SICL-ENVIRONMENT:GLOBAL-ENVIRONMENT is defined to take an
;;; optional parameter, and if that optional parameter is supplied, it
;;; calls TRUCLER:GLOBAL-ENVIRONMENT, so that is what we do here,
;;; since we always intend to supply an environment parameter.  But 
;;; TRUCLER:GLOBAL-ENVIRONMENT requires a CLIENT parameter.
;;;
;;; The only way to avoid this circular dependency is to require a
;;; CLIENT parameter to SICL-ENVIRONMENT:GET-SETF-EXPANSION.  For
;;; consistency with Clostrum functions, we use the same order of the
;;; parameters as Clostrum does, i.e., CLIENT and ENVIRONMENT before
;;; any remaining parameters.

(defun get-setf-expansion (client environment place)
  (let* ((global-env (trucler:global-environment client environment)))
    (if (symbolp place)
        (let ((description (trucler:describe-variable client environment place)))
          (if (typep description 'trucler:symbol-macro-description)
              (get-setf-expansion client environment (trucler:expansion description))
              (let ((temp (gensym)))
                (values '() '() `(,temp) `(setq ,place ,temp) place))))
        (let ((description
                (trucler:describe-function client environment (first place))))
          (if (typep description 'trucler:macro-description)
              (let* ((expander (trucler:expander description))
                     (expansion (funcall expander place)))
                (get-setf-expansion client environment expansion))
              (let ((expander (setf-expander client global-env (first place))))
                (if (null expander)
                    (let ((temps (mapcar (lambda (p) (declare (ignore p)) (gensym))
                                         (rest place)))
                          (new (gensym)))
                      (values temps
                              (rest place)
                              (list new)
                              `(funcall (function (setf ,(first place))) ,new ,@temps)
                              `(,(first place) ,@temps)))
                    (funcall expander environment place))))))))
