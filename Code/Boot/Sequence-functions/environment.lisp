(cl:in-package #:sicl-boot-sequence-functions)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%environment :initarg :environment :reader environment)
   (%base :initarg :base :reader base)))

(defparameter *sequence-function-names*
  '(subseq (setf subseq)
    map
    map-into
    reduce
    count count-if count-if-not
    length
    reverse nreverse
    sort stable-sort
    find find-iif find-if-not
    position position-if position-if-not
    search
    mismatch
    replace
    substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not
    concatenate
    merge
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    remove-duplicates delete-duplicates))

(defmethod env:fdefinition
    ((client client) (environment environment) function-name)
  (let* ((base (base client))
         (base-client (env:client base))
         (result (env:fdefinition base-client environment function-name)))
    (if (null result)
        ;; Then we query the base environment instead.  And we make
        ;; sure to use the base client so that we avoid infinite
        ;; computations.
        (env:fdefinition base-client base function-name)
        ;; Otherwise, we return the result.
        result)))

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

(defmethod env:function-cell
    ((client client) (environment environment) function-name)
  (let* ((base (base client))
         (base-client (env:client base))
         (local-definition (env:fdefinition base-client environment function-name)))
    (if (null local-definition)
        ;; Then we query the base environment instead.  And we make
        ;; sure to use the base client so that we avoid infinite
        ;; computations.
        (env:function-cell base-client base function-name)
        ;; Otherwise, we query the local environment, but we make sure
        ;; to use the base client.
        (env:function-cell base-client environment function-name))))

(defmethod env:function-cell
    ((client client) (environment sicl-boot-phase-5:environment) function-name)
  ;; See whether FUNCTION-NAME has a definition in the
  ;; environment dedicated to this phase.
  (if (null (env:fdefinition client (environment client) function-name))
      ;; It does not, so then just use the one from ENVIRONMENT.
      (call-next-method)
      ;; It does, so take the function cell from the environment
      ;; dedicated to this phase.
      (env:function-cell client (environment client) function-name)))

(defmethod (setf env:fdefinition)
    (new-definition (client client) (environment sicl-boot-phase-5:environment) function-name)
  ;; We always want to set the definition in the environment dedicated
  ;; to this phase.
  (setf (env:fdefinition client (environment client) function-name)
        new-definition))

(defmethod (setf env:constant-variable)
    (new-value (client client) (environment environment) symbol)
  (let* ((base (base client))
         (base-client (env:client base)))
    (setf (env:constant-variable base-client base symbol) new-value)))
