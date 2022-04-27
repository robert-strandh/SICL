(cl:in-package #:sicl-boot-sequence-functions)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%environment :initarg :environment :reader environment)))

(defmethod env:fdefinition
    ((client client) (environment sicl-boot-phase-5:environment) function-name)
  (let (;; See whether FUNCTION-NAME has a definition in the
        ;; environment dedicated to this phase.
        (result (env:fdefinition client (environment client) function-name)))
    (if (null result)
        ;; If there is no definition of FUNCTION-NAME in the
        ;; environment dedicated to this phase, then we check whether
        ;; the name is that of a sequence function.
        (if (member function-name *sequence-function-names* :test #'equal)
            ;; If that is the case, then we return NIL, indicating
            ;; that the function is not defined at all, so as to avoid
            ;; returning a host function from E5.
            nil
            ;; If not, then we return whatever E5 might have as a
            ;; definition for FUNCTION-NAME.
            (call-next-method))
        ;; If there is a definition of FUNCTION-NAME in the
        ;; environment dedicated to this phase, then we return it.
        result)))

(defmethod (setf env:fdefinition)
    (new-definition (client client) (environment sicl-boot-phase-5:environment) function-name)
  ;; We always want to set the definition in the environment dedicated
  ;; to this phase.
  (setf (env:fdefinition client (environment client) function-name)
        new-definition))
