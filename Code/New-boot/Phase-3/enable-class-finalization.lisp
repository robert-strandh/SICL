(cl:in-package #:sicl-new-boot-phase-3)

;;; In phase 3, the purpose of class finalization is to finalize the
;;; bridge classes in E2, so that we can create ersatz generic
;;; functions in E4, and ersatz classes in E3.  In other words, we are
;;; using accessors that operate on the bridge classes in E2, and
;;; those accessors are found in E2 as well.  For that reason, most of
;;; the code in this file refers to E2.

(defun enable-class-finalization (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    (setf (sicl-genv:special-variable
           'sicl-clos::*standard-direct-slot-definition* e2 t)
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e1))
    (setf (sicl-genv:special-variable
           'sicl-clos::*standard-effective-slot-definition* e2 t)
          (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition e1))
    (sicl-genv:fmakunbound 'sicl-clos:direct-slot-definition-class e2)
    (load-file "CLOS/slot-definition-class-support.lisp" e2)
    (load-file "CLOS/slot-definition-class-defgenerics.lisp" e2)
    (load-file "CLOS/slot-definition-class-defmethods.lisp" e2)
    (load-file "CLOS/class-finalization-defgenerics.lisp" e2)
    ;; FIND-IF-NOT is used to traverse the list of direct slot
    ;; definitions to find the first one that has a non-null
    ;; initfunction, which will then be used as the initfunction of
    ;; the effective slot-definition.
    (import-function-from-host 'find-if-not e2)
    ;; MAPCAR is used for several things during class finalization.
    ;; It is used to obtain the class-precedence-list of all
    ;; superclasses of a class.  It is used to find the initargs of
    ;; each slot definition object in a list.  And more.
    (import-function-from-host 'mapcar e2)
    ;; REMOVE-DUPCLICATES is used in serveral places for class
    ;; finalization, for computing a list of superclasses, a list of
    ;; initargs, a list of slots, etc.
    (import-function-from-host 'remove-duplicates e2)
    ;; UNION is used by class finalization to compute a list of
    ;; effective slot definitions.
    (import-function-from-host 'union e2)
    ;; REDUCE is used by class finalization in several places to combine
    ;; lists of superclasses, slots, and initargs from superclasses.
    (import-function-from-host 'reduce e2)
    ;; COUNT is used by class finalization to determine the number of
    ;; slots with :INSTANCE allocation.
    (import-function-from-host 'count e2)
    ;; Although LAST is not mentioned explicitly, it is needed, because
    ;; the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'last e2)
    ;; Although COPY-LIST is not mentioned explicitly, it is needed,
    ;; because the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'copy-list e2)
    (load-file "CLOS/class-finalization-support.lisp" e2)
    (load-file "CLOS/class-finalization-defmethods.lisp" e2)))
