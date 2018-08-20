(cl:in-package #:sicl-new-boot-phase-2)

;;; We transform a form like (DEFMETHOD ... NAME ((X CLASS) ...) BODY)
;;; into (DEFMETHOD <NAME> ((X <CLASS>) ...) BODY, where <NAME> and
;;; <CLASS> are generated symbols so that <NAME> is fbound in the
;;; global host environment has the function named by NAME in ENV2 and
;;; so that <CLASS> in the global host environment names the class
;;; named by CLASS in ENV1.  Then we wrap the new form in an form that
;;; will call EVAL of the host to evalute the newly constructed
;;; DEFMETHOD form.
(defun define-defmethod-expander (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:defmethod-expander env2)
        (lambda (ct-env name rest)
          (declare (ignore ct-env))
          (multiple-value-bind
                (qualifiers
                 required
                 remaining
                 specializers
                 declarations
                 documentation
                 forms)
              (sicl-clos::parse-defmethod rest)
            (declare (ignore declarations documentation))
            (let* ((temp-name (gensym))
                   (temps
                     (loop for specializer in specializers
                           collect (if (or (not (symbolp specializer))
                                           (eq specializer t))
                                       specializer
                                       (let ((name (gensym)))
                                         (setf (find-class name)
                                               (sicl-genv:find-class
                                                specializer env1))
                                         name))))
                   (lambda-list
                     (append (loop for arg in required
                                   for specializer in temps
                                   collect `(,arg ,specializer))
                             remaining)))
              (setf (fdefinition temp-name)
                    (sicl-genv:fdefinition name env2))
              (list 'eval-defmethod
                    `'(defmethod ,temp-name ,@qualifiers ,lambda-list
                        ,@forms))))))
  (setf (sicl-genv:fdefinition 'eval-defmethod env2)
        #'eval))

(defun boot-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos e3)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defclass-expander e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     '(setf sicl-genv:special-variable) e2)
    (define-defmethod-expander e1 e2)
    (load-accessor-defgenerics boot)
    (create-mop-classes boot)
    (load-file "CLOS/defmethod-defmacro.lisp" e2)))
