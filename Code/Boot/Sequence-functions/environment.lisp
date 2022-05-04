(cl:in-package #:sicl-boot-sequence-functions)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client) ())

(defparameter *sequence-function-names*
  '(subseq (setf subseq)
    elt (setf elt)
    map
    map-into
    reduce
    count count-if count-if-not
    length
    reverse nreverse
    sort stable-sort
    find find-if find-if-not
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

(defvar *environment*)

(defmethod env:fdefinition
    ((client client)
     (environment sicl-boot-phase-5:environment)
      function-name)
  (if (member function-name *sequence-function-names* :test #'equal)
      ;; Then query the environment dedicated to this phase instead.
      (env:fdefinition client *environment* function-name)
      ;; Else do the default thing.
      (call-next-method)))

(defmethod (setf env:fdefinition)
    (new-function
     (client client)
     (environment sicl-boot-phase-5:environment)
     function-name)
  (if (member function-name *sequence-function-names* :test #'equal)
      ;; Then define it in the environment dedicated to this phase
      ;; instead.
      (setf (env:fdefinition client *environment* function-name)
            new-function)
      ;; Else do the default thing.
      (call-next-method)))

(defmethod cleavir-cst-to-ast:declaration-proclamations
    ((client sicl-boot:client) (environment sicl-boot-phase-5:environment))
  '(fast-generic-functions:method-properties))
