(cl:in-package #:sicl-boot-phase-4)

;;; In phase 4, the purpose of class finalization is to finalize the
;;; bridge classes in E3, so that we can create ersatz generic
;;; functions in E4, and ersatz classes in E4.  In other words, we are
;;; using accessors that operate on the bridge classes in E3, and
;;; those accessors are found in E3 as well.  For that reason, most of
;;; the code in this file refers to E3.

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (setf (sicl-genv:fdefinition
           'sicl-clos::effective-slot-definition-class-default e3)
          (lambda (class &rest initargs)
            (declare (ignore class initargs))
            (sicl-genv:find-class
             'sicl-clos:standard-effective-slot-definition e2)))
    (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" e3)
    (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" e3)))

(defun define-class-finalization (boot)
  (with-accessors ((e3 sicl-boot:e3)) boot
    (load-fasl "CLOS/class-finalization-defgenerics.fasl" e3)
    ;; FIND-IF-NOT is used to traverse the list of direct slot
    ;; definitions to find the first one that has a non-null
    ;; initfunction, which will then be used as the initfunction of
    ;; the effective slot-definition.
    (import-function-from-host 'find-if-not e3)
    ;; MAPCAR is used for several things during class finalization.
    ;; It is used to obtain the class-precedence-list of all
    ;; superclasses of a class.  It is used to find the initargs of
    ;; each slot definition object in a list.  And more.
    (import-function-from-host 'mapcar e3)
    ;; REMOVE-DUPCLICATES is used in serveral places for class
    ;; finalization, for computing a list of superclasses, a list of
    ;; initargs, a list of slots, etc.
    (import-function-from-host 'remove-duplicates e3)
    ;; UNION is used by class finalization to compute a list of
    ;; effective slot definitions.
    (import-function-from-host 'union e3)
    ;; REDUCE is used by class finalization in several places to combine
    ;; lists of superclasses, slots, and initargs from superclasses.
    (import-function-from-host 'reduce e3)
    ;; COUNT is used by class finalization to determine the number of
    ;; slots with :INSTANCE allocation.
    (import-function-from-host 'count e3)
    ;; Although LAST is not mentioned explicitly, it is needed, because
    ;; the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'last e3)
    ;; Although COPY-LIST is not mentioned explicitly, it is needed,
    ;; because the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'copy-list e3)
    (load-fasl "CLOS/class-finalization-support.fasl" e3)
    (load-fasl "CLOS/class-finalization-defmethods.fasl" e3)))

(defun enable-class-finalization (boot)
  (define-effective-slot-definition-class boot)
  (define-class-finalization boot))
